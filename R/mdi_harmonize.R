#' Harmonize an NSI data file to MD specification
#'
#' @description
#' Applies a data file's rows of the NSI-to-MD concordance, method by method:
#' \describe{
#'   \item{revalue}{`detail` is an expression in `x`, the NSI variable, e.g.
#'     `log(x)`, `x*1000` or `format(as.Date(x, "%Y%m%d"), "%Y")`.}
#'   \item{recode / reclass}{`detail` names the code table (codeconc or
#'     classconc) that maps the variable's values, `left` to `right`.}
#'   \item{redefine}{builds an MD variable from several NSI variables:
#'     `detail` is `+` or `-` (NA-safe sum or difference) or `fn(...)`.}
#'   \item{remap}{renames the NSI variable to its MD name.}
#' }
#' It then keeps only the MD variables and sets `year`: from the file's year
#' variable if it has one (a panel), otherwise from the concordance.
#'
#' @param DT data.table, one NSI data file. Modified by reference.
#' @param concordance data.table, the concordance rows for this file:
#'   `NSI_dataset`, `NSI_varname`, `MD_varname`, `method`, `detail`, `year`.
#' @param country character(1), the country code. A `detail` starting with it
#'   names an NSI metadata table, which [mdi_load_nsi_metadata()] loads under
#'   an `NSI` prefix instead (`FI_MD_codeconc` -> `NSI_MD_codeconc`).
#' @param code_tables named list of the code tables recode/reclass rows can
#'   name: the NSI and MD metadata, e.g. `c(cfg$nsi, cfg$md)`.
#' @param year_var character(1), the file's year variable, or `NA`/`""` if it
#'   has none.
#'
#' @return `DT`, harmonized.
#' @examples
#' raw <- data.table::data.table(TURN = c(1, 2), EMP = c(3, 4))
#' conc <- data.table::data.table(
#'   NSI_dataset = "sbs", year = 2020,
#'   NSI_varname = c("TURN", "TURN", "EMP"),
#'   MD_varname = c("turnover", "turnover", "emp"),
#'   method = c("revalue", "remap", "remap"),
#'   detail = c("x*1000", NA, NA)
#' )
#' mdi_harmonize(raw, conc, "FI")
#' @export
mdi_harmonize <- function(DT, concordance, country, code_tables = list(), year_var = NA_character_) {
  check_dt(DT)
  check_dt(concordance, c("NSI_dataset", "NSI_varname", "MD_varname", "method", "detail", "year"),
           "concordance")
  check_string(country, "country")
  if (!is.list(code_tables)) stop("'code_tables' must be a named list of data.tables.")
  if (!(length(year_var) == 1L && (is.na(year_var) || is.character(year_var))))
    stop("'year_var' must be a single variable name, or NA.")

  missing <- setdiff(concordance[["NSI_varname"]], names(DT))
  if (length(missing)) {
    stop("mdi_harmonize(): the concordance maps variable(s) ", paste(missing, collapse = ", "),
         ", which are not in the data (NSI dataset ",
         paste(unique(concordance[["NSI_dataset"]]), collapse = ", "), ", year ",
         paste(unique(concordance[["year"]]), collapse = ", "), ").", call. = FALSE)
  }

  methods <- concordance[["method"]]

  ########################
  ####### revalue #######
  ########################
  if (any(methods == "revalue")) {
    message("Step 1: revalue")
    conc1 <- concordance[methods == "revalue"]
    for (i in seq_len(nrow(conc1))) {
      varname <- conc1[["NSI_varname"]][i]
      # x stands for the variable
      expr_str <- gsub("\\bx\\b", varname, conc1[["detail"]][i], perl = TRUE)
      mdi_check_safe_expr(expr_str)
      DT[, (varname) := eval(parse(text = expr_str))]
    }
    message("Revalue done")
  }

  ########################
  ### recode / reclass ###
  ########################
  if (any(methods %in% c("recode", "reclass"))) {
    message("Step 2/3: recode/reclass")
    conc2 <- concordance[methods %in% c("recode", "reclass")]
    # recode rows whose table has no codes for them: their values stay as
    # they are, which the run should say rather than pass on silently
    no_codes <- character(0)
    for (i in seq_len(nrow(conc2))) {
      varname <- conc2[["NSI_varname"]][i]
      detail <- conc2[["detail"]][i]
      codeconc <- .harmonize_code_table(detail, code_tables, country, varname)

      # a codeconc holds several variables' codes
      if (grepl("_codeconc", detail)) {
        codeconc <- codeconc[codeconc[["NSI_varname"]] == varname &
                               codeconc[["NSI_dataset"]] == conc2[["NSI_dataset"]][i]]
      }
      # codes that differ by year: this file's year
      if (length(unique(codeconc[["year"]])) > 1) {
        codeconc <- codeconc[codeconc[["year"]] %in% conc2[["year"]]]
      }
      if (nrow(codeconc) == 0) {
        no_codes <- c(no_codes, sprintf("%s (NSI dataset %s, year %s, table %s)", varname,
                                        conc2[["NSI_dataset"]][i], conc2[["year"]][i], detail))
      }

      # values without a code in the table keep their value
      recoding_levels <- as.character(codeconc[["left"]])
      recoding_labels <- as.character(codeconc[["right"]])
      unmatched_levels <- setdiff(unique(DT[[varname]]), recoding_levels)
      combined_levels <- c(recoding_levels[!is.na(recoding_levels)], unmatched_levels[!is.na(unmatched_levels)])
      combined_labels <- c(recoding_labels[!is.na(recoding_labels)], unmatched_levels[!is.na(unmatched_levels)])
      if (length(combined_levels) < length(combined_labels)) {
        stop("Mismatch between the number of elements in code/class NSI_varname ", varname,
             " using file ", detail, ".")
      }
      set(DT, j = varname, value = factor(DT[[varname]], levels = combined_levels, labels = combined_labels))
    }
    if (length(no_codes) > 0) {
      warning("mdi_harmonize(): no codes in the code table for ", paste(no_codes, collapse = "; "),
              ". Their values were kept as they are, not recoded.", call. = FALSE)
    }
    message("Reclass/code done")
  }

  ########################
  ####### redefine #######
  ########################
  if (any(methods == "redefine")) {
    message("Step 4: redefine")
    conc4 <- concordance[methods == "redefine"]

    # new variables get a temporary prefix until all are built, so one can't
    # overwrite an NSI variable another still needs
    tmp_var_prefix <- format(Sys.time(), "%Y%m%d%H%M%S_")

    # each row's sign applies to its variable; the first variable is taken
    # as it is (detail "+" or "-" alike), as in "+A -B -C" = A - B - C. NAs
    # count as 0, the result is NA only where every variable is NA.
    generate_na_safe_expr <- function(varright, vars, signs) {
      if (!all(signs %in% c("+", "-"))) {
        stop("mdi_harmonize(): redefine of '", varright, "' mixes '+'/'-' rows with other details: ",
             paste(unique(signs), collapse = ", "), ".", call. = FALSE)
      }
      terms <- paste0(c("", paste0(" ", signs[-1], " ")), "tmp_vars[[", seq_along(vars), "]]", collapse = "")
      paste0(
        "DT[, paste0(tmp_var_prefix, '", varright, "') := {",
        "tmp_vars <- lapply(.SD, function(x) fifelse(is.na(as.numeric(x)), 0, as.numeric(x)));",
        "res <- ", terms, ";",
        "all_na <- Reduce(`&`, lapply(.SD, is.na));",
        "fifelse(all_na, NA_real_, res)",
        "}, .SDcols = c(", paste0("'", vars, "'", collapse = ", "), ")]"
      )
    }

    for (varright in unique(conc4[["MD_varname"]])) {
      subset_conc <- conc4[conc4[["MD_varname"]] == varright]
      expr <- if (any(subset_conc[["detail"]] %in% c("+", "-"))) {
        generate_na_safe_expr(varright, subset_conc[["NSI_varname"]], subset_conc[["detail"]])
      } else {
        # fn(...): the expression inside
        paste0("DT[, paste0(tmp_var_prefix, '", varright, "') := ",
               sub(".*fn\\((.*)\\).*", "\\1", unique(subset_conc[["detail"]]), perl = TRUE), "]")
      }
      mdi_check_safe_expr(expr)
      eval(parse(text = expr))
    }

    setnames(DT, old = names(DT), new = gsub(tmp_var_prefix, "", names(DT)))
    message("Redefine done")
  }

  ########################
  ######## remap #########
  ########################
  message("Step 5: remap")
  conc5 <- concordance[methods == "remap"]
  setnames(DT, old = conc5[["NSI_varname"]], new = conc5[["MD_varname"]])

  # Keep only the MD variables, plus the file's year variable if it has one
  # (a panel) and it wasn't remapped already
  keep <- intersect(names(DT), unique(concordance[["MD_varname"]]))
  has_year_var <- .is_set(year_var)
  if (has_year_var && year_var %in% names(DT) && !(year_var %in% keep)) keep <- c(year_var, keep)
  DT <- DT[, keep, with = FALSE]
  message("Remap done")

  # set(): DT$year <- would copy the whole table once more
  if (!has_year_var) {
    set(DT, j = "year", value = as.integer(unique(concordance[["year"]])))
  } else {
    setnames(DT, old = year_var, new = "year", skip_absent = TRUE)
    set(DT, j = "year", value = as.integer(DT[["year"]]))
  }

  DT
}

# The code table a recode/reclass row names in `detail`. NSI metadata tables
# are loaded with the country prefix replaced by "NSI".
.harmonize_code_table <- function(detail, code_tables, country, varname) {
  if (is.na(detail) || detail == "") {
    stop("mdi_harmonize(): the recode/reclass row for ", varname,
         " doesn't name a code table in 'detail'.")
  }
  key <- if (startsWith(detail, country)) sub(country, "NSI", detail, fixed = TRUE) else detail
  if (is.null(code_tables[[key]])) {
    stop("mdi_harmonize(): the recode/reclass row for ", varname, " names code table '",
         detail, "', which isn't loaded (looked for '", key, "' in 'code_tables').")
  }
  code_tables[[key]]
}
