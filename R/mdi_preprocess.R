# ---------------------------------------------------------------------------
# parsedate helper
# Month names are parsed through readr (part of the MDI package stack), whose
# locale() bundles CLDR month names for ~185 languages: no month dictionaries
# are hardcoded here and parsing never depends on the system locale.
# ---------------------------------------------------------------------------
mdi_parsedate <- function(x, informat, lang = "en", outformat = NULL) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("parsedate: the 'readr' package is required (it is part of the MDI package stack)")
  }
  loc <- tryCatch(readr::locale(lang), error = function(e) {
    stop("parsedate: unknown language code '", lang,
         "'. Valid codes: see readr::date_names_langs().")
  })

  s <- trimws(as.character(x))
  empty <- is.na(s) | s == ""

  # readr matches month names case-insensitively for ASCII only: normalize the
  # case explicitly (plain lowercase + accented uppercase -> lowercase) so that
  # e.g. "OTT"/"Ott"/"ott" and "MÄRZ"/"märz" all match the CLDR forms
  if (grepl("%b|%B", informat)) {
    s <- chartr("\u00C0\u00C1\u00C2\u00C3\u00C4\u00C5\u00C7\u00C8\u00C9\u00CA\u00CB\u00CC\u00CD\u00CE\u00CF\u00D1\u00D2\u00D3\u00D4\u00D5\u00D6\u00D9\u00DA\u00DB\u00DC\u00DD",
                "\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5\u00E7\u00E8\u00E9\u00EA\u00EB\u00EC\u00ED\u00EE\u00EF\u00F1\u00F2\u00F3\u00F4\u00F5\u00F6\u00F9\u00FA\u00FB\u00FC\u00FD",
                tolower(s))
  }

  # readr::parse_date is strict: unparseable values AND silent mis-parses
  # (e.g. a 4-digit year read with %y) come back as NA
  d <- suppressWarnings(readr::parse_date(s, format = informat, locale = loc))

  failed <- !empty & is.na(d)
  if (any(failed)) {
    if (all(failed[!empty])) {
      stop("parsedate: no non-empty value of the column parses with informat '",
           informat, "' and language '", lang, "'. Examples: ",
           paste(utils::head(unique(x[failed]), 5), collapse = ", "))
    }
    warning("parsedate: ", sum(failed), " value(s) could not be parsed with '",
            informat, "' and were set to NA. Examples: ",
            paste(utils::head(unique(x[failed]), 5), collapse = ", "))
  }

  if (is.null(outformat) || outformat == "") d else {
    out <- format(d, outformat)
    out[is.na(d)] <- NA_character_
    out
  }
}

# ---------------------------------------------------------------------------
# Typed preprocessing operations. Each takes already-parsed, typed arguments
# (real R vectors/lists, not pipe/hash-delimited strings) and does exactly
# one operation on dt. mdi_preprocess() below is the only
# caller: it parses the instruction-string DSL and dispatches to these.
# ---------------------------------------------------------------------------

#' Deduplicate `dt` by `unique_vars`, using `method` to pick the surviving row.
#' @noRd
prep_dedup <- function(dt, unique_vars, method, dedupvar = NULL, seed = 1L) {
  if (method == "random") {
    # the same row on every run; the caller's random numbers are restored
    .with_local_seed(seed)
    return(.keep_row_per_group(dt, unique_vars, function(rows) rows[sample(length(rows), 1)]))
  }

  # If there are duplicates, keep the row with the least number of NAs.
  # If records have the exact same information, it just selects one among them.
  if (!anyDuplicated(dt, by = unique_vars)) return(dt)

  if (method == "na") {
    na_count <- Reduce(`+`, lapply(dt, is.na))
    dt <- .keep_row_per_group(dt, unique_vars, function(rows) rows[which.min(na_count[rows])])
  }

  if (method %in% c("max", "min")) {
    pick <- if (method == "max") which.max else which.min
    ref <- dt[[dedupvar]]
    # pick() returns integer(0) when the reference column is all NA in a
    # group, which would silently DROP that group: keep the first row instead
    dt <- .keep_row_per_group(dt, unique_vars, function(rows) {
      i <- pick(ref[rows])
      rows[if (length(i)) i else 1L]
    })
  }

  if (method == "meanmode") {
    col_order <- names(dt)
    group_cols <- unique_vars
    data_cols <- setdiff(names(dt), group_cols)

    num_cols <- Filter(function(col) is.numeric(dt[[col]]), data_cols)
    cat_cols <- setdiff(data_cols, num_cols)

    mean_dt <- if (length(num_cols) > 0) {
      dt[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = group_cols, .SDcols = num_cols]
    }

    mode_dt <- if (length(cat_cols) > 0) {
      dt[, lapply(.SD, .mode), by = group_cols, .SDcols = cat_cols]
    } else {
      NULL
    }

    if (!is.null(mean_dt) && !is.null(mode_dt)) {
      dt <- merge(mean_dt, mode_dt, by = group_cols, all = TRUE)
    } else if (!is.null(mean_dt)) {
      dt <- mean_dt
    } else if (!is.null(mode_dt)) {
      dt <- mode_dt
    } else {
      stop("No numeric or categorical columns found to aggregate.")
    }

    dt <- dt[, col_order, with = FALSE]
  }

  dt
}

# One row per group of `by`, chosen by choose(row numbers of the group): the
# groups in order of first appearance, the `by` columns first -- the layout of
# dt[, .SD[...], by = by], without building .SD for every group.
.keep_row_per_group <- function(dt, by, choose) {
  rows <- dt[, list(.row = choose(.I)), by = by][[".row"]]
  out <- dt[rows]
  setcolorder(out, by)
  out
}

#' Filter `dt` rows where `colname <operator> value` holds.
#' @noRd
prep_filter <- function(dt, colname, operator, value = NULL) {
  # isna/notna test for missingness and take no value
  if (operator == "isna") return(dt[is.na(get(colname))])
  if (operator == "notna") return(dt[!is.na(get(colname))])

  value <- gsub("^['\"]|['\"]$", "", value)

  value_vector <- if (operator %in% c("in", "nin")) {
    strsplit(value, "#")[[1]]
  } else {
    value
  }

  if (operator == "eq") {
    dt[get(colname) == value_vector]
  } else if (operator == "neq") {
    dt[get(colname) != value_vector]
  } else if (operator == "gt") {
    dt[get(colname) > as.numeric(value_vector)]
  } else if (operator == "gte") {
    dt[get(colname) >= as.numeric(value_vector)]
  } else if (operator == "lt") {
    dt[get(colname) < as.numeric(value_vector)]
  } else if (operator == "lte") {
    dt[get(colname) <= as.numeric(value_vector)]
  } else if (operator == "in") {
    dt[get(colname) %in% value_vector]
  } else if (operator == "nin") {
    dt[!(get(colname) %in% value_vector)]
  } else {
    stop("Unsupported operator: ", operator)
  }
}

#' Aggregate `dt` by `group_cols`. `agg_list` maps column -> function name
#' (or `"mode"`); `custom_pickmax` maps column -> the column to pick its
#' max-row value from.
#' @noRd
prep_agg <- function(dt, group_cols, agg_list, custom_pickmax) {
  # each column split into its groups and summarised in base R: evaluating
  # an R function per group inside data.table was most of the run
  groups <- dt[, list(.grp = .GRP, .row = .I), by = group_cols]
  grp <- integer(nrow(dt))
  grp[groups[[".row"]]] <- groups[[".grp"]]
  by_group <- function(x) split(x, grp)

  out <- unique(dt[, group_cols, with = FALSE], by = group_cols)
  for (col in names(agg_list)) {
    func_name <- agg_list[[col]]
    value <- if (func_name == "mode") {
      as.character(.mode_by_group(dt[[col]], grp, nrow(out)))
    } else {
      func <- match.fun(func_name)
      # missing values are skipped, and a group with only missing values gives
      # NA rather than 0 (sum) or Inf (min/max); na.rm is passed to every
      # function, as formals() can't detect it for primitives like sum/min/max
      vapply(by_group(dt[[col]]), function(x) {
        if (all(is.na(x))) NA_real_ else as.numeric(func(x, na.rm = TRUE))
      }, numeric(1))
    }
    set(out, j = col, value = unname(value))
  }
  for (col in names(custom_pickmax)) {
    ref <- dt[[custom_pickmax[[col]]]]
    # a group whose reference column is all NA has no row to pick: NA, as
    # for the other functions on an all-NA group (the group is kept)
    rows <- vapply(by_group(seq_len(nrow(dt))), function(r) {
      i <- which.max(ref[r])
      if (length(i)) r[i] else NA_integer_
    }, integer(1))
    set(out, j = col, value = dt[[col]][unname(rows)])
  }
  out
}

# .mode() of x within each group 1..n_groups, counted over the whole vector
# at once. Values are counted in order of first appearance within their
# group and the sort is stable, so a tie goes to the first to appear.
.mode_by_group <- function(x, grp, n_groups) {
  ok <- which(!is.na(x))
  counts <- data.table(g = grp[ok], v = x[ok])[, list(N = .N), by = c("g", "v")]
  setorderv(counts, c("g", "N"), c(1L, -1L))
  top <- unique(counts, by = "g")
  out <- x[rep(NA_integer_, n_groups)]
  out[top[["g"]]] <- top[["v"]]
  out
}

#' Drop `var_to_remove` from `dt`, then deduplicate on `unique_vars`.
#' @noRd
prep_restruct <- function(dt, var_to_remove, unique_vars) {
  dt[, (var_to_remove) := NULL]
  unique(dt[, unique_vars, with = FALSE])
}

#' Reshape `dt` long-to-wide via `data.table::dcast`.
#' @noRd
prep_reshape <- function(dt, id_vars, names_from_vars, value_vars) {
  rhs <- paste(names_from_vars, collapse = " + ")
  formula <- as.formula(paste(paste(id_vars, collapse = " + "), "~", rhs))
  dcast(dt, formula, value.var = value_vars)
}

#' Reshape `dt` wide-to-long via `data.table::melt`: each of `measure_vars`
#' becomes a row, its name in `names_to` and its value in `values_to`.
#' @noRd
prep_reshapelong <- function(dt, id_vars, measure_vars, names_to, values_to) {
  # variable.factor = FALSE keeps the new names column as character, which is
  # what recode/reclass expect downstream
  melt(dt, id.vars = id_vars, measure.vars = measure_vars, variable.name = names_to,
       value.name = values_to, variable.factor = FALSE)
}

#' Derive `new_col` in `dt` from an ordered condition -> value map, falling
#' back to `default_value` where no condition matched.
#' @noRd
prep_derive <- function(dt, new_col, cond_val_pairs, default_value) {
  if (!(new_col %in% names(dt))) {
    dt[, (new_col) := NA_character_]
  }

  for (i in seq_along(cond_val_pairs)) {
    condition <- names(cond_val_pairs)[i]
    value <- cond_val_pairs[[i]]

    mdi_check_safe_expr(condition)
    dt[
      eval(parse(text = condition)) & is.na(get(new_col)),
      (new_col) := if (value %in% names(dt)) get(value) else value
    ]
  }

  dt[
    is.na(get(new_col)),
    (new_col) := if (default_value %in% names(dt)) get(default_value) else default_value
  ]

  dt
}

#' Scale `target_cols` in `dt` by a per-value factor, where `factor_map`
#' maps a value of `condition_col` to its multiplier.
#' @noRd
prep_scaleif <- function(dt, condition_col, factor_map, target_cols) {
  for (val in names(factor_map)) {
    factor <- factor_map[[val]]
    dt[get(condition_col) == val,
       (target_cols) := lapply(.SD, function(x) x * factor),
       .SDcols = target_cols
    ]
  }
  dt
}

#' Trim the last `trim_n` characters from each of `target_cols` in `dt`.
#' @noRd
prep_trimchars <- function(dt, target_cols, trim_n) {
  for (col in target_cols) {
    if (!col %in% names(dt)) {
      warning(paste("Column", col, "not found in data. Skipping."))
      next
    }

    if (!is.character(dt[[col]])) {
      warning(paste("Column", col, "is not a character column. Coercing to character."))
      dt[, (col) := as.character(get(col))]
    }

    dt[, (col) := substr(get(col), 1, pmax(nchar(get(col)) - trim_n, 0))]
  }
  dt
}

#' Parse/normalize `date_col` in `dt` via `mdi_parsedate()`.
#' @noRd
prep_parsedate <- function(dt, date_col, informat, lang = "en", outformat = NULL) {
  if (!date_col %in% names(dt)) {
    stop("parsedate: column '", date_col, "' not found in the datafile")
  }
  dt[, (date_col) := mdi_parsedate(get(date_col), informat, lang, outformat)]
  dt
}

#' Read `other_datafile` with `read_datafile` and merge `merge_cols` into
#' `dt` on `join_keys`.
#' @noRd
prep_mergefrom <- function(dt, other_datafile, join_keys, merge_cols, read_datafile,
                           join_type = "left") {
  join_type <- tolower(join_type)
  if (!join_type %in% c("left", "inner", "full")) {
    stop("mergefrom: join_type must be one of: left, inner, full")
  }

  # ---- 1) Read the other file ----
  # join keys that are text here are read as text there too: a number-like
  # reading would lose leading zeros, or stop the join on mixed types
  keep_other <- unique(c(join_keys, merge_cols))
  text_keys <- join_keys[vapply(join_keys, function(k) is.character(dt[[k]]), logical(1))]
  other_dt <- read_datafile(other_datafile, keep_other, text_keys)

  # ---- 2) Column checks ----
  missing_other <- setdiff(keep_other, names(other_dt))
  if (length(missing_other) > 0) {
    stop("mergefrom: '", other_datafile, "' lacks column(s): ", paste(missing_other, collapse = ", "))
  }
  other_dt <- other_dt[, keep_other, with = FALSE]

  # ---- 3) Join keys of the same kind on both sides ----
  mixed <- join_keys[vapply(join_keys, function(k) is.character(dt[[k]]) != is.character(other_dt[[k]]),
                            logical(1))]
  if (length(mixed) > 0) {
    stop("mergefrom: join key(s) ", paste(mixed, collapse = ", "), " are text on one side and numbers ",
         "on the other ('", other_datafile, "'). Declare them with the same class in <CC>_varnames.csv.")
  }

  # ---- 4) Enforce uniqueness on BOTH sides ----
  if (anyDuplicated(dt, by = join_keys)) {
    stop("mergefrom: dt has duplicate rows for join keys (must be unique).")
  }

  if (anyDuplicated(other_dt, by = join_keys)) {
    stop("mergefrom: other_dt has duplicate rows for join keys (must be unique).")
  }

  # ---- 5) Prevent name collisions (besides keys) ----
  collisions <- intersect(setdiff(names(other_dt), join_keys), names(dt))
  if (length(collisions) > 0) {
    stop(
      "mergefrom: column name collision(s) with dt: ",
      paste(collisions, collapse = ", "),
      ". Rename columns before mergefrom or only merge non-colliding columns."
    )
  }

  # ---- 6) Perform join ----
  setkeyv(dt, join_keys)
  setkeyv(other_dt, join_keys)

  if (join_type == "left") {
    other_dt[dt]  # x[i] keeps i
  } else if (join_type == "inner") {
    other_dt[dt, nomatch = 0]
  } else if (join_type == "full") {
    merge(dt, other_dt, by = join_keys, all = TRUE)
  }
}

#' Preprocessing Tool for Raw Data Harmonization
#'
#' This function applies one or more preprocessing operations to a `data.table` object.
#' It is designed to prepare raw microdata files for harmonization by applying a flexible
#' set of transformations through a structured instruction string.
#'
#' The input `preprocessing_string` is a single string where:
#' - Multiple operations are separated by double pipes (`||`)
#' - Parameters within each operation are separated by pipes (`|`)
#' - When a parameter contains multiple elements (e.g., multiple columns), they are separated by a hash symbol (`#`)
#'
#' **IMPORTANT:** When performing more than one operation, **order matters**. The result of each step is fed into the next.
#'
#' Supported operations:
#'
#' - `dedup|<id_col1#id_col2#...>|<method>|[dedup_col]`
#'     Removes duplicates based on identifier columns.
#'     Methods:
#'     - `na`: keep the row with the fewest missing values
#'     - `max` / `min`: keep the row with the max/min value of the specified `dedup_col`
#'     - `meanmode`: aggregate numeric columns using the mean, and categorical columns using the mode
#'     - `random`: keep one row among duplicates at random (the same row on every run)
#'
#' - `filter|<col>|<operator>|<value>`
#'     Filters rows based on a logical condition.
#'     Operators:
#'     - `eq`, `neq`: equal / not equal
#'     - `gt`, `gte`, `lt`, `lte`: greater/less than (or equal to)
#'     - `in`, `nin`: value in (or not in) a list, separated by `#` (e.g., `US#IT#DE`)
#'     - `isna`, `notna`: keep rows where the column is / is not missing. These
#'       take **no value**, so the instruction has three parts, not four
#'       (e.g., `filter|FIRM_ID|notna` to drop rows with no firm identifier).
#'
#' - `agg|<group_col1#group_col2#...>|<var1:func#var2:func#...>`
#'     Aggregates rows to a lower dimension by grouping.
#'     Functions supported: `sum`, `mean`, `min`, `max`, `median`, `sd`, `mode`, `pickmaxby-refcol`
#'
#' - `restruct|<col_to_remove>|<col1#col2#...>`
#'     Drops a column that introduces duplication and deduplicates on a set of remaining columns.
#'
#' - `reshape|<id_col1#id_col2#...>|<names_from>|<values_from>`
#'     Reshapes data from long to wide format using `data.table::dcast`.
#'
#' - `reshapelong|<id_col1#id_col2#...>|<measure_col1#measure_col2#...>|<names_to>|<values_to>`
#'     Reshapes data from wide to long format using `data.table::melt`: each
#'     measure column becomes a row. `names_to` is the new column holding the
#'     original column name, `values_to` the new column holding its value.
#'     Useful when a raw file stores one record per category in separate
#'     columns but the MD schema expects one row per category.
#'
#'     Example (trade data with one column per direction, to one row per
#'     direction, then drop the empty ones):
#'     `reshapelong|FIRM_ID#PRODUCT#COUNTRY|import#export|FLOW|VALUE||filter|VALUE|notna`
#'
#' - `derive|<new_col>|condmap|<cond1>:<val1>#<cond2>:<val2>#...|default:<default_val>`
#'     Creates a new column based on a sequence of conditional expressions.
#'     - Conditions are evaluated in order and the first match is used.
#'     - The condition (`condX`) is written in standard R syntax (e.g., `colA == 1 & colB == 'X'`).
#'     - The value (`valX`) can be a column name or a static value.
#'     - A fallback value must be provided via `default:<value>`.
#'
#'     Example:
#'     `derive|new_col|condmap|type == 2:colA#year == 2008 & type == 1:colB|default:NA`
#'
#' - `scaleif|<condition_col>|<val1:factor1#val2:factor2#...>|<col1#col2#...>`
#'     Conditionally scales one or more columns by a numeric factor depending on the value of a categorical column.
#'     - The second argument is a mapping of `value:factor` pairs (e.g., `1:1#2:1000#5:100`).
#'     - The third argument lists the columns to be scaled when the condition is met.
#'
#'     Example:
#'     `scaleif|unit_code|1:1#2:1000#3:0.001|value#quantity`
#'
#' - `trimchars|<col1#col2#...>|<n>`
#'     Trims the last `n` characters from each specified character column.
#'     Useful for standardizing identifier or code formats.
#'
#'     Example:
#'     `trimchars|prodcom_code|2`
#'
#' - `parsedate|<col>|<informat>|[lang]|[outformat]`
#'     Parses a date column given its layout in strptime tokens (e.g. `%d-%b-%y`),
#'     independently of the system locale. Month names (`%b` abbreviated / `%B`
#'     full) are matched through `readr`'s bundled CLDR month names: `lang` is a
#'     two-letter language code (`it`, `de`, `fr`, ... - see
#'     `readr::date_names_langs()` for the ~185 available); English if omitted.
#'     Case is normalized first, so "31-OTT-12" and "31-ott-12" both parse.
#'     By default the column is replaced by a `Date`; an optional `outformat`
#'     (e.g. `%Y%m%d`) returns a formatted string instead. Values that do not
#'     parse are set to NA with a warning; if no non-empty value parses the
#'     operation stops.
#'
#'     Example (balance-sheet closing dates with Italian month names like
#'     "31-OTT-12", then keep the latest balance sheet per firm):
#'     `parsedate|CLOSING_DATE|%d-%b-%y|it||dedup|FIRM_ID|max|CLOSING_DATE`
#'
#' - `mergefrom|<datafile_name>|<join_key1#join_key2#...>|<col1#col2#...>|[join_type]`
#'     Reads the join keys and the listed columns of another raw file,
#'     `datafile_name` (an `NSI_datafile` of `NSI_datafiles`), with
#'     `read_datafile` (see below), and merges the columns into the current
#'     dataset on the join keys. The other file's own preprocessing is not run.
#'
#'     The join type is optional and defaults to a **left join**.
#'     Supported join types:
#'     - `left`: keep all rows from the current dataset (default)
#'     - `inner`: keep only rows with matching keys in both datasets
#'     - `full`: full outer join; keep all rows from both datasets
#'
#'     The join keys must uniquely identify rows in **both** datasets, and
#'     be text on both sides or numbers on both sides. If a key combination
#'     is missing on either side, the corresponding values are set to `NA`.
#'
#'     Example:
#'     `mergefrom|sbs_2020|FIRM_ID#Jahr|NACE|left`
#'
#' @param preprocessing_string A character string defining one or more preprocessing instructions
#' @param dt A `data.table` containing the raw input dataset to be processed
#' @param read_datafile A function `function(datafile, cols, text_cols)`
#'   returning the columns `cols` of the raw file `datafile` as a
#'   `data.table`, with `text_cols` read as text. Only required when
#'   `preprocessing_string` contains a `mergefrom` instruction; the pipeline
#'   reads the file with its own `NSI_datafiles` row (path, format, encoding,
#'   Oracle) and its own declared classes.
#'
#' @return A `data.table` with the specified preprocessing operations applied
#'
#' @examples
#' dt <- data.table::data.table(firm_id = c(1, 1, 2), year = 2020,
#'                              country = c("IT", "IT", "FR"), emp = c(5, 6, 7))
#' # one row per firm and year (the one with most employment), Italian firms only
#' mdi_preprocess("dedup|firm_id#year|max|emp||filter|country|eq|IT", dt)
#'
#' @export
mdi_preprocess <- function(preprocessing_string, dt, read_datafile = NULL) {
  # each step's result is fed into the next
  for (step in .parse_preprocessing(preprocessing_string)) {
    .check_prepro_columns(step, names(dt))
    args <- step$args
    if (step$op == "mergefrom") {
      if (!is.function(read_datafile)) {
        stop("mergefrom: mdi_preprocess() requires read_datafile to be supplied to use mergefrom")
      }
      args <- c(args, list(read_datafile = read_datafile))
    }
    # the parsed arguments are named after prep_<op>()'s parameters
    dt <- do.call(get(paste0("prep_", step$op), mode = "function"), c(list(dt), args))
  }
  dt
}

# ---------------------------------------------------------------------------
# The instruction-string DSL, parsed once for every caller:
# mdi_preprocess() runs the steps, .prepro_needed_vars() and
# .import_spec() read which columns they use. Returns one list(op, args) per
# step, args named after prep_<op>()'s parameters; stops on anything it
# does not recognise, instead of silently skipping the step.
# ---------------------------------------------------------------------------
.parse_preprocessing <- function(preprocessing_string) {
  if (!.is_set(preprocessing_string)) return(list())
  lapply(strsplit(preprocessing_string, "||", fixed = TRUE)[[1]], .parse_prepro_step)
}

.prepro_ops <- c("dedup", "filter", "agg", "restruct", "reshape", "reshapelong", "derive",
                 "scaleif", "trimchars", "parsedate", "mergefrom")

# The functions agg|...|<col>:<fun> may call; anything else would run any R
# function a metadata file names.
.prepro_agg_funs <- c("sum", "mean", "median", "min", "max", "sd", "mode")

.parse_prepro_step <- function(instruction) {
  parts <- strsplit(instruction, "|", fixed = TRUE)[[1]]
  op <- parts[1]
  p <- parts[-1]
  hash <- function(x) if (is.na(x)) character(0) else strsplit(x, "#", fixed = TRUE)[[1]]
  fail <- function(...) stop("preprocessing step '", instruction, "': ", ..., call. = FALSE)

  if (!op %in% .prepro_ops) {
    fail("unknown operation '", op, "'; supported: ", paste(.prepro_ops, collapse = ", "), ".")
  }

  args <- switch(op,
    dedup = {
      methods <- c("na", "max", "min", "meanmode", "random")
      if (!p[2] %in% methods) fail("dedup method must be one of: ", paste(methods, collapse = ", "), ".")
      if (p[2] %in% c("max", "min") && is.na(p[3])) fail("dedup|", p[2], " needs the column to compare.")
      list(unique_vars = hash(p[1]), method = p[2],
           dedupvar = if (p[2] %in% c("max", "min")) p[3])
    },
    filter = {
      # isna/notna take no value: one part fewer
      if (p[2] %in% c("isna", "notna")) {
        if (length(p) != 2) fail("filter with '", p[2], "' takes no value: filter|column|", p[2])
        list(colname = p[1], operator = p[2])
      } else {
        if (length(p) != 3) fail("filter instruction must have exactly 3 parts: filter|column|operator|value")
        list(colname = p[1], operator = p[2], value = p[3])
      }
    },
    agg = {
      agg_list <- list()
      custom_pickmax <- list()
      for (inst in hash(p[2])) {
        col_func <- strsplit(inst, ":", fixed = TRUE)[[1]]
        if (length(col_func) < 2) fail("invalid aggregation instruction '", inst, "'.")
        func <- paste(col_func[-1], collapse = ":")
        if (!startsWith(func, "pickmaxby-") && !func %in% .prepro_agg_funs) {
          fail("aggregation function '", func, "' isn't supported; use one of: ",
               paste(.prepro_agg_funs, collapse = ", "), ", or pickmaxby-<column>.")
        }
        if (startsWith(func, "pickmaxby-")) {
          custom_pickmax[[col_func[1]]] <- sub("^pickmaxby-", "", func)
        } else {
          agg_list[[col_func[1]]] <- func
        }
      }
      list(group_cols = hash(p[1]), agg_list = agg_list, custom_pickmax = custom_pickmax)
    },
    restruct = list(var_to_remove = p[1], unique_vars = hash(p[2])),
    reshape = list(id_vars = hash(p[1]), names_from_vars = hash(p[2]), value_vars = hash(p[3])),
    reshapelong = {
      if (length(p) != 4) fail("reshapelong needs the form reshapelong|<id_cols>|<measure_cols>|<names_to>|<values_to>.")
      list(id_vars = hash(p[1]), measure_vars = hash(p[2]), names_to = p[3], values_to = p[4])
    },
    derive = {
      if (!identical(p[2], "condmap")) fail("derive needs the form derive|<new_col>|condmap|<cond>:<val>#...|default:<val>.")
      if (length(p) < 4 || !startsWith(p[4], "default:")) {
        fail("must provide exactly one default value using the format default:<value>")
      }
      pairs <- hash(p[3])
      pairs <- pairs[!startsWith(pairs, "default:")]
      cond_val_pairs <- character(0)
      for (pair in pairs) {
        cv <- strsplit(pair, ":", fixed = TRUE)[[1]]
        if (length(cv) != 2) fail("each condition must be in the format condition:value")
        cond_val_pairs[cv[1]] <- cv[2]
      }
      list(new_col = p[1], cond_val_pairs = cond_val_pairs,
           default_value = sub("^default:", "", p[4]))
    },
    scaleif = {
      pairs <- hash(p[2])
      list(condition_col = p[1],
           factor_map = setNames(as.numeric(sub(".*:", "", pairs)), sub(":.*", "", pairs)),
           target_cols = hash(p[3]))
    },
    trimchars = list(target_cols = hash(p[1]), trim_n = as.integer(p[2])),
    parsedate = {
      lang <- if (is.na(p[3])) "" else p[3]
      outformat <- if (is.na(p[4])) "" else p[4]
      # the third piece may be the outformat when the language is omitted
      # (outformats contain '%'; language codes do not)
      if (grepl("%", lang, fixed = TRUE) && outformat == "") {
        outformat <- lang
        lang <- ""
      }
      list(date_col = p[1], informat = p[2], lang = if (lang == "") "en" else lang,
           outformat = outformat)
    },
    mergefrom = list(other_datafile = p[1], join_keys = hash(p[2]), merge_cols = hash(p[3]),
                     join_type = if (is.na(p[4])) "left" else p[4])
  )
  list(op = op, args = args, text = instruction)
}

# Stops if a step reads a column the data doesn't have (checked against the
# data as the earlier steps left it), instead of failing obscurely or, for
# dedup's max/min, silently keeping the first row. A derive value or default
# that looks like a column name but isn't one is written as text: warned.
.check_prepro_columns <- function(step, cols) {
  a <- step$args
  need <- switch(step$op,
    trimchars = character(0),           # warns and skips a missing column itself
    restruct = a$unique_vars,           # dropping an absent column is harmless
    mergefrom = a$join_keys,            # the merge columns come from the other file
    derive = {
      # conditions may only name columns: anything else would be looked up
      # outside the data (e.g. data.table's year())
      used <- unlist(lapply(names(a$cond_val_pairs), function(cond) all.vars(str2lang(cond))))
      setdiff(used, c("T", "F", "Inf", "NaN", "pi"))
    },
    .prepro_step_vars(step)
  )
  missing <- setdiff(need, cols)
  if (length(missing)) {
    stop("preprocessing step '", step$text, "': column(s) ", paste(missing, collapse = ", "),
         " not in the data.", call. = FALSE)
  }
  if (step$op == "derive") {
    values <- c(unname(a$cond_val_pairs), a$default_value)
    as_text <- unique(values[grepl("^[A-Za-z_][A-Za-z0-9_.]*$", values) &
                               !toupper(values) %in% c("NA", "TRUE", "FALSE") & !values %in% cols])
    if (length(as_text)) {
      warning("preprocessing step '", step$text, "': ", paste(as_text, collapse = ", "),
              " is not a column of the data, so it is written as text.", call. = FALSE)
    }
  }
}
