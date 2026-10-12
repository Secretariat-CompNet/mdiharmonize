#' mdi_key_id_switch
#'
#' @title Switch Identifier Level via Aggregation or Disaggregation
#'
#' @description
#' Performs aggregation or disaggregation of a harmonized dataset across
#' hierarchical identifier levels using concordance tables and metadata-driven rules.
#'
#' The supported identifier hierarchy is:
#'
#' \preformatted{
#' plantid -> firmid -> entid -> entgrp
#' }
#'
#' The function relies on `MD_varnames` to identify keys, variable structure,
#' and — via its `aggregation_method`/`disaggregation_method` columns — how each
#' variable should be transformed, and on concordance tables (`*_conc.csv`) to
#' map between identifier levels.
#'
#' @param DT A `data.table` containing the harmonized dataset to transform.
#'   Must include exactly one firm identifier column (`plantid`, `firmid`,
#'   `entid`, or `entgrp`) and a `year` column.
#' @param conctab A `data.table` concordance table mapping source identifier
#'   to target identifier, with columns for both id levels and `year`.
#' @param md_varnames A `data.table`, `MD_varnames` already filtered to the
#'   current dataset (e.g. `MD_varnames[MD_dataset == "BR"]`). Passing
#'   unfiltered `MD_varnames` (spanning more than one `MD_dataset`) errors.
#' @param method Character. `"agg"` to aggregate from lower to higher
#'   identifier level, or `"disagg"` to disaggregate from higher to lower.
#' @param obj_id_type Character. Target identifier level: `"to_firmid"`,
#'   `"to_entid"`, `"to_entgrp"`, or `"to_plantid"`.
#' @param cols Optional character vector of columns to process. Default
#'   `NULL` (all columns).
#' @param key_id Character. Name of the source identifier column in `DT`
#'   (e.g. `"plantid"`, `"firmid"`). Default `"firmid"`.
#' @param weight_data Named list of `data.table`s, the MD datasets the run has
#'   (e.g. `list(SBS = sbs, BR = br, PRODCOM = prodcom)`), for the weights of
#'   `"weighted_alloc"` disaggregation. `NULL` (default): every
#'   `"weighted_alloc"` variable is split equally, with a warning. Only the
#'   target id, `year` and the weight columns are used; the tables are not
#'   modified.
#'
#' @return A `data.table` with variables aggregated or disaggregated to the
#'   requested identifier level.
#'
#' @details
#' Variable-specific transformation rules are looked up from the
#' `aggregation_method`/`disaggregation_method` columns in `MD_varnames`.
#'
#' For aggregation (`method = "agg"`), multiple lower-level units are combined
#' into a higher-level unit, for example from plant to firm. Each variable is
#' aggregated according to its `aggregation_method`.
#'
#' Supported aggregation methods include `"sum"`, `"weighted_avg"`,
#' `"weighted_mode"`, `"mode"`, `"any"`, `"all"`, `"min"`, and `"max"`.
#'
#' For disaggregation (`method = "disagg"`), higher-level observations are split
#' into lower-level units using concordance mappings, for example from firm to plant.
#' Each variable is transformed according to its `disaggregation_method`.
#'
#' Supported disaggregation methods include `"replicate"` and `"weighted_alloc"`.
#'
#' Aggregation preserves totals for additive variables such as those using `"sum"`.
#' Disaggregation with `"replicate"` does not preserve totals. Disaggregation with
#' `"weighted_alloc"` approximately preserves totals. Factor variables are internally
#' converted to character to avoid unintended integer behavior.
#'
#' Rows whose `key_id` the concordance has no target unit for are dropped,
#' with a message counting them per year. In aggregation, a group whose
#' values are all `NA` stays `NA` (not 0 for `"sum"`, or `FALSE`/`TRUE` for
#' `"any"`/`"all"`).
#'
#' Survey-weight variables (`weights_<dataset>_nsi`) are dropped before the
#' switch, with a warning: they belong to the sample, not to the firm.
#'
#' If an aggregation rule names a weight column (`"weighted_avg:w"`,
#' `"weighted_mode:w1|w2"`) and none of the named column(s) are present in
#' `DT`, a warning is raised once upfront and the variable falls back to an
#' unweighted mean/mode.
#'
#' A `"weighted_alloc"` rule lists its weights in order, each as
#' `<MD_dataset>.<MD_varname>`, e.g.
#' `"weighted_alloc:SBS.emp|PRODCOM.rev|BR.persons_br|equal"`. The chain is
#' in `MD_varnames` so every module splits a variable the same way. A weight
#' is that column of `weight_data[[<MD_dataset>]]`, summed per target unit
#' and year; it can't be a column of `DT` itself, which holds the source
#' unit's value on every target row. For each source unit and year, the first
#' candidate with weights that aren't all missing or zero for its target
#' units wins; if none does, the value is split equally. A candidate whose
#' dataset isn't in `weight_data`, isn't at the target level or lacks the
#' column is skipped, with one warning naming it. A message reports, per
#' variable, how many source units used each candidate.
#'
#' @examples
#' \dontrun{
#' # conctab must be a data.table mapping plantid -> firmid with a year column
#' ener_firm <- mdi_key_id_switch(DT = ener_dt, conctab = plant_firm_conc,
#'                                md_varnames = MD_varnames[MD_dataset == "ENER"],
#'                                method = "agg",
#'                                obj_id_type = "to_firmid",
#'                                key_id = "plantid")
#'
#' # firm-level BS split to plants, weighted as MD_varnames' chains say
#' bs_plant <- mdi_key_id_switch(DT = bs_dt, conctab = plant_firm_conc,
#'                               md_varnames = MD_varnames[MD_dataset == "BS"],
#'                               method = "disagg",
#'                               obj_id_type = "to_plantid",
#'                               key_id = "firmid",
#'                               weight_data = list(SBS = sbs, BR = br, PRODCOM = prodcom))
#' }
#'
#' @export

mdi_key_id_switch <- function(DT, conctab, md_varnames, method,
                              obj_id_type, cols = NULL, key_id = "firmid",
                              weight_data = NULL) {
  check_dt(DT)
  check_dt(conctab)
  check_dt(md_varnames)
  if ("MD_dataset" %in% names(md_varnames) &&
      data.table::uniqueN(md_varnames$MD_dataset) > 1) {
    stop(
      "md_varnames must be filtered to a single dataset before calling ",
      "mdi_key_id_switch() -- e.g. MD_varnames[MD_dataset == \"BR\"]. ",
      "Found ", data.table::uniqueN(md_varnames$MD_dataset), " distinct MD_dataset values."
    )
  }
  if (!method %in% c("agg", "disagg"))
    stop("'method' must be \"agg\" or \"disagg\"")
  if (!is.null(weight_data) && (!is.list(weight_data) || is.data.frame(weight_data) ||
                                is.null(names(weight_data)) || any(!nzchar(names(weight_data))))) {
    stop("'weight_data' must be a named list of data.tables, e.g. list(SBS = sbs, BR = br).")
  }

  if (!is.null(cols)) {
    DT <- DT[, cols, with = FALSE]
  }

  # ------------------------------------------------------------
  # Drop survey-weight variables if present
  # ------------------------------------------------------------
  # every MD dataset's survey weight is named weights_<dataset>_nsi
  present_survey_weights <- grep("^weights_[a-z0-9]+_nsi$", names(DT), value = TRUE)

  if (length(present_survey_weights) > 0) {
    warning(
      "The following survey-weight variables are present in the data and will be removed before ID switching because they should not be aggregated or disaggregated: ",
      paste(present_survey_weights, collapse = ", ")
    )
    DT[, (present_survey_weights) := NULL]
  }

  # Load aggregation/disaggregation metadata
  DT_cols <- md_varnames[md_varnames[["MD_varname"]] %in% names(DT)]

  # ---- Build observation keys ----
  obs_keys_all <- md_varnames[["MD_varname"]][which(md_varnames[["is_key"]] == 1)]
  firm_id_keys <- .firm_id_levels

  # firm id present in this dataset
  present_firm_id <- intersect(firm_id_keys, names(DT))
  if (length(present_firm_id) != 1) {
    stop(
      paste(
        "Expected exactly one firm identifier in data, found:",
        paste(present_firm_id, collapse = ", ")
      )
    )
  }

  # non-firm keys (e.g. year, product codes, countries)
  other_keys <- setdiff(obs_keys_all, firm_id_keys)

  # final observation keys
  obs_keys <- c(present_firm_id, intersect(other_keys, names(DT)))

  # ---- Concordance ----
  obj_id <- gsub("to_", "", obj_id_type)

  # Make sure key columns are characters before merging
  key_id_obj_id <- c(key_id, obj_id)
  # the caller's concordance stays as it was
  conctab <- data.table::copy(conctab)
  conctab[, (key_id_obj_id) := lapply(.SD, as.character), .SDcols = key_id_obj_id]

  ###############################################
  ### 1. Aggregation
  ###############################################
  if (method == "agg") {

    if (!all(c(key_id, "year") %in% names(DT))) {
      stop(paste0("Source data does not contain required merge keys: ", key_id, " and/or year"))
    }

    DT <- merge(DT, conctab, by = c(key_id, "year"), all.x = TRUE)

    if (!obj_id %in% names(DT)) {
      stop("Concordance merge failed: target obj_id was not added to DT")
    }
    DT <- .kis_drop_unmatched(DT, key_id, obj_id)

    vars_to_aggregate <- .switch_vars(DT_cols, names(DT), c(obs_keys, obj_id))
    var_rules <- .switch_rules(DT_cols, vars_to_aggregate, "aggregation_method")

    if (length(vars_to_aggregate) == 0) {
      stop("No variables left to aggregate after excluding keys and target id.")
    }

    .kis_require_rules(vars_to_aggregate, var_rules, "aggregation")

    .kis_warn_missing_weight_cols(var_rules, vars_to_aggregate, names(DT),
                                  c("weighted_avg", "weighted_mode"), "aggregation")

    for (col in vars_to_aggregate) {
      rule_type <- sub(":.*", "", var_rules[[col]])
      if ((is.character(DT[[col]]) || is.factor(DT[[col]])) &&
          !rule_type %in% c("mode", "weighted_mode")) {
        warning("Aggregation rule '", var_rules[[col]], "' is not valid for text column '",
                col, "'; using mode instead.")
        var_rules[[col]] <- "mode"
      }
    }

    # yes/no columns stay logical, so any/all aggregate them as such
    for (col in vars_to_aggregate) {
      if (is.factor(DT[[col]])) set(DT, j = col, value = as.character(DT[[col]]))
    }

    aggregated_DT <- DT[, {
      agg_result <- list()
      for (col_name in vars_to_aggregate) {
        agg_result[[col_name]] <- .kis_agg_fun(.SD, col_name, var_rules[[col_name]])
      }
      as.list(agg_result)
    }, by = c(obj_id, obs_keys[obs_keys != key_id]), .SDcols = vars_to_aggregate]

    return(aggregated_DT)
  }

  ###############################################
  ### 2. Disaggregation
  ###############################################
  if (method == "disagg") {
    if (obj_id %in% names(DT)) {
      stop(paste0("You're trying to disaggregate to ", obj_id, " but data is already at that level."))
    }

    # Merge at source level
    DT <- merge(DT, conctab, by = c(key_id, "year"), all.x = TRUE)
    DT <- .kis_drop_unmatched(DT, key_id, obj_id)

    vars_to_disagg <- .switch_vars(DT_cols, names(DT), c(obs_keys, obj_id))
    var_rules <- .switch_rules(DT_cols, vars_to_disagg, "disaggregation_method")

    if (length(vars_to_disagg) == 0) {
      stop("No variables left to disaggregate after excluding keys and target id.")
    }

    .kis_require_rules(vars_to_disagg, var_rules, "disaggregation")

    # the weights of weighted_alloc rules, per target unit and year, under
    # internal names that can't clash with DT's own columns
    weights <- .kis_alloc_weights(var_rules[vars_to_disagg], weight_data, obj_id)
    for (w in weights) {
      DT <- merge(DT, w, by = c(obj_id, "year"), all.x = TRUE)
    }
    weight_cols <- unlist(lapply(weights, function(w) setdiff(names(w), c(obj_id, "year"))))
    tally <- new.env()

    for (col in vars_to_disagg) {
      if (is.factor(DT[[col]])) {
        set(DT, j = col, value = as.character(DT[[col]]))
      }
    }

    disaggregated_DT <- DT[, {
      dest_ids <- unique(.SD[[obj_id]])
      n_units <- length(dest_ids)

      base_cols <- setdiff(names(.SD), c(obj_id, vars_to_disagg, weight_cols))
      base_row <- .SD[1L, base_cols, with = FALSE]
      base_out <- base_row[rep(1L, n_units)]
      base_out[, (obj_id) := dest_ids]

      res <- setNames(vector("list", length(vars_to_disagg)), vars_to_disagg)
      for (col_name in vars_to_disagg) {
        result <- .kis_disagg_fun(.SD, col_name, var_rules[[col_name]], obj_id, dest_ids)
        used <- attr(result, "weight")
        if (!is.null(used)) {
          key <- paste(col_name, used, sep = "\r")
          tally[[key]] <- (if (is.null(tally[[key]])) 0L else tally[[key]]) + 1L
          attr(result, "weight") <- NULL
        }

        if (length(result) != length(dest_ids)) {
          stop(paste0("Length mismatch in ", col_name))
        }

        if (!is.atomic(result)) {
          stop(paste0("Non atomic result in ", col_name))
        }

        res[[col_name]] <- result
      }

      cbind(base_out, as.data.table(res))
    }, by = obs_keys]

    .kis_report_alloc(tally)
    return(disaggregated_DT)
  }
}

# The MD variables in the data to switch: all but the observation keys and
# the target id.
.switch_vars <- function(dt_cols, data_names, keys) {
  v <- dt_cols[["MD_varname"]]
  unique(v[v %in% data_names & !v %in% keys])
}

# Each variable's aggregation or disaggregation rule, named by variable.
.switch_rules <- function(dt_cols, vars, rule_col) {
  in_vars <- dt_cols[["MD_varname"]] %in% vars
  setNames(dt_cols[[rule_col]][in_vars], dt_cols[["MD_varname"]][in_vars])
}

# ---- helpers of mdi_key_id_switch() ----

# Rows the concordance has no target unit for can't be assigned to one; kept,
# they would form one unit with an NA id per year. Dropped, with their count
# per year.
.kis_drop_unmatched <- function(DT, key_id, obj_id) {
  unmatched <- is.na(DT[[obj_id]])
  if (!any(unmatched)) return(DT)
  per_year <- table(DT[["year"]][unmatched])
  message("mdi_key_id_switch(): ", sum(unmatched), " row(s) with a ", key_id, " the concordance has no ",
          obj_id, " for, dropped. By year: ",
          paste0(names(per_year), ": ", as.integer(per_year), collapse = ", "), ".")
  DT[!unmatched]
}

# The internal name of a weighted_alloc candidate's weight column.
.kis_weight_col <- function(cand) paste0(".w.", cand)

# The weights the weighted_alloc rules name, as "<MD_dataset>.<MD_varname>":
# that column of weight_data[[<MD_dataset>]], summed per target unit and
# year, one table per candidate. Warns once about candidates that can't be
# used (dataset not passed, not at the target level, column missing).
.kis_alloc_weights <- function(rules, weight_data, obj_id) {
  rules <- rules[!is.na(rules) & startsWith(rules, "weighted_alloc")]
  cands <- unique(unlist(lapply(strsplit(sub("^weighted_alloc:?", "", rules), "|", fixed = TRUE), trimws)))
  cands <- setdiff(cands[nzchar(cands)], "equal")
  out <- list()
  unusable <- character(0)
  for (cand in cands) {
    parts <- strsplit(cand, ".", fixed = TRUE)[[1]]
    ds <- parts[1]
    var <- if (length(parts) == 2) parts[2] else NA_character_
    src <- if (!is.na(var)) weight_data[[ds]]
    why <- if (is.na(var)) "not of the form <MD_dataset>.<MD_varname>"
      else if (is.null(src)) paste(ds, "not in weight_data")
      else if (!obj_id %in% names(src)) paste(ds, "not at", obj_id, "level")
      else if (!all(c("year", var) %in% names(src))) paste(ds, "has no", if (var %in% names(src)) "year" else var)
    if (!is.null(why)) {
      unusable <- c(unusable, sprintf("%s (%s)", cand, why))
      next
    }
    # summed per target unit and year: a dataset can have several rows per
    # unit (e.g. PRODCOM's products)
    w <- data.table(id = as.character(src[[obj_id]]), year = src[["year"]],
                    w = suppressWarnings(as.numeric(src[[var]])))
    w <- w[, list(w = if (all(is.na(w))) NA_real_ else sum(w, na.rm = TRUE)), by = c("id", "year")]
    data.table::setnames(w, c("id", "w"), c(obj_id, .kis_weight_col(cand)))
    out[[cand]] <- w
  }
  if (length(unusable) > 0) {
    warning("weighted_alloc weight(s) that can't be used, skipped: ", paste(unusable, collapse = "; "),
            if (length(out) == 0) ". Every weighted_alloc variable is split equally.", call. = FALSE)
  }
  out
}

# One message per weighted_alloc variable: how many source units each weight
# split, from the counts .kis_disagg_fun()'s results carry.
.kis_report_alloc <- function(tally) {
  keys <- ls(tally)
  if (length(keys) == 0) return(invisible())
  parts <- do.call(rbind, strsplit(keys, "\r", fixed = TRUE))
  n <- vapply(keys, function(k) tally[[k]], integer(1))
  for (v in unique(parts[, 1])) {
    mine <- parts[, 1] == v
    message("mdi_key_id_switch(): ", v, " split by ",
            paste(sprintf("%s for %d", parts[mine, 2], n[mine]), collapse = ", "), " source unit(s).")
  }
}

# Every variable to switch needs a rule.
.kis_require_rules <- function(vars, var_rules, label) {
  missing <- vars[!vars %in% names(var_rules) | is.na(var_rules[vars]) | var_rules[vars] == ""]
  if (length(missing) > 0) {
    stop("Missing ", label, " methods for: ", paste(missing, collapse = ", "))
  }
}

# Warns once, upfront, for any variable whose weighted aggregation rule
# ("weighted_avg:w", "weighted_mode:w1|w2") has none of its candidate weight
# column(s) in the data -- .kis_agg_fun() would otherwise fall back to an
# unweighted mean/mode silently, once per group. (weighted_alloc weights are
# checked in .kis_alloc_weights().)
.kis_warn_missing_weight_cols <- function(var_rules, vars, dt_names, weighted_types, mode_label) {
  missing <- character(0)
  for (v in vars) {
    rule <- var_rules[[v]]
    if (is.na(rule) || rule == "") next
    parts <- strsplit(rule, ":", fixed = TRUE)[[1]]
    if (!parts[1] %in% weighted_types) next
    candidates <- if (length(parts) > 1) strsplit(parts[2], "\\|")[[1]] else NULL
    if (length(candidates) == 0 || !any(candidates %in% dt_names)) {
      missing <- c(missing, sprintf("%s (expected one of: %s)", v, paste(candidates, collapse = ", ")))
    }
  }
  if (length(missing) > 0) {
    warning(
      "The following variable(s) use a weighted ", mode_label,
      " method but none of their weight column(s) are present in the data -- falling back to an unweighted method: ",
      paste(missing, collapse = "; ")
    )
  }
}

.kis_typed_na <- function(template) {
  if (is.character(template) || is.factor(template)) return(NA_character_)
  if (is.logical(template)) return(NA_real_)
  if (inherits(template, "Date")) return(as.Date(NA))
  if (inherits(template, "POSIXt")) return(as.POSIXct(NA))
  return(NA_real_)
}

.kis_coerce_like <- function(template, x) {
  if (is.character(template) || is.factor(template)) return(as.character(x))
  # any/all/mode of a yes/no column stay yes/no; a sum of one is a count
  if (is.logical(template)) return(if (is.logical(x)) x else as.numeric(x))
  if (inherits(template, "Date")) return(as.Date(x))
  if (inherits(template, "POSIXt")) return(as.POSIXct(x))
  return(as.numeric(x))
}

.kis_safe_min <- function(x) {
  if (all(is.na(x))) return(.kis_typed_na(x))
  if (inherits(x, "Date")) return(min(as.Date(x), na.rm = TRUE))
  if (inherits(x, "POSIXt")) return(min(as.POSIXct(x), na.rm = TRUE))
  return(min(as.numeric(x), na.rm = TRUE))
}

.kis_safe_max <- function(x) {
  if (all(is.na(x))) return(.kis_typed_na(x))
  if (inherits(x, "Date")) return(max(as.Date(x), na.rm = TRUE))
  if (inherits(x, "POSIXt")) return(max(as.POSIXct(x), na.rm = TRUE))
  return(max(as.numeric(x), na.rm = TRUE))
}

# A group with only NAs stays NA, of the type the rule gives (else sum
# gives 0, any FALSE, all TRUE, mean NaN).
.kis_agg_fun <- function(data, col_name, rule) {
  val <- .kis_agg_rule(data, col_name, rule)
  if (all(is.na(data[[col_name]]))) val[] <- NA
  val
}

.kis_agg_rule <- function(data, col_name, rule) {
  templ <- data[[col_name]]

  if (is.na(rule) || rule == "") rule <- "mode"
  parts <- strsplit(rule, ":", fixed = TRUE)[[1]]
  type <- parts[1]
  candidates <- if (length(parts) > 1) strsplit(parts[2], "\\|")[[1]] else NULL

  if (type == "sum") {
    val <- sum(as.numeric(data[[col_name]]), na.rm = TRUE)
    return(.kis_coerce_like(templ, val))
  }

  if (type == "weighted_avg") {
    for (cand in candidates) {
      if (cand %in% names(data)) {
        # as weighted_mode: only weights that add up to more than 0, rows
        # without a weight or value left out
        x <- as.numeric(data[[col_name]])
        w <- as.numeric(data[[cand]])
        ok <- !is.na(x) & !is.na(w)
        if (sum(w[ok]) > 0) {
          return(.kis_coerce_like(templ, weighted.mean(x[ok], w[ok])))
        }
      }
    }
    val <- mean(as.numeric(data[[col_name]]), na.rm = TRUE)
    return(.kis_coerce_like(templ, val))
  }

  if (type == "weighted_mode") {
    for (cand in candidates) {
      if (cand %in% names(data)) {
        w <- as.numeric(data[[cand]])
        if (sum(w, na.rm = TRUE) > 0) {
          # codes in order of first appearance, so a tie goes to the first
          # to appear, as for mode
          codes <- data[[col_name]]
          ok <- !is.na(codes)
          first <- unique(codes[ok])
          tot <- tapply(w[ok], factor(codes[ok], levels = first), sum, na.rm = TRUE)
          return(.kis_coerce_like(templ, first[which.max(tot)]))
        }
      }
    }
    return(.kis_coerce_like(templ, .mode(data[[col_name]])))
  }

  if (type == "any") {
    val <- any(data[[col_name]], na.rm = TRUE)
    return(.kis_coerce_like(templ, val))
  }

  if (type == "all") {
    val <- all(data[[col_name]], na.rm = TRUE)
    return(.kis_coerce_like(templ, val))
  }

  if (type == "mode") {
    return(.kis_coerce_like(templ, .mode(data[[col_name]])))
  }

  if (type == "min") {
    return(.kis_coerce_like(templ, .kis_safe_min(data[[col_name]])))
  }

  if (type == "max") {
    return(.kis_coerce_like(templ, .kis_safe_max(data[[col_name]])))
  }

  stop(paste("Unknown aggregation rule:", rule))
}

.kis_cast_like <- function(template, x) {
  if (is.factor(template)) return(as.character(x))
  if (is.logical(template)) return(as.logical(x))
  if (is.integer(template)) return(as.integer(x))
  if (is.numeric(template)) return(as.numeric(x))
  if (is.character(template)) return(as.character(x))
  if (inherits(template, "Date")) return(as.Date(x, origin = "1970-01-01"))
  if (inherits(template, "POSIXt")) return(as.POSIXct(x, origin = "1970-01-01"))
  x
}

.kis_disagg_fun <- function(g, col_name, rule, obj_id, dest_ids) {
  if (is.na(rule) || rule == "") rule <- "replicate"
  parts <- strsplit(rule, ":")[[1]]
  type <- parts[1]
  cands <- if (length(parts) > 1) strsplit(parts[2], "\\|")[[1]] else NULL

  templ <- g[[col_name]]
  n <- length(dest_ids)

  vals <- unique(g[[col_name]])
  vals <- vals[!is.na(vals)]

  if (length(vals)) {
    x <- vals[1]
  } else {
    if (is.factor(templ)) {
      x <- NA_character_
    } else if (is.logical(templ)) {
      x <- NA
    } else if (is.integer(templ)) {
      x <- NA_integer_
    } else if (is.numeric(templ)) {
      x <- NA_real_
    } else if (is.character(templ)) {
      x <- NA_character_
    } else if (inherits(templ, "Date")) {
      x <- as.Date(NA)
    } else if (inherits(templ, "POSIXt")) {
      x <- as.POSIXct(NA)
    } else {
      x <- NA
    }
  }

  if (type == "replicate") {
    return(.kis_cast_like(templ, rep(x, n)))
  }

  if (type == "weighted_alloc") {
    if (!is.numeric(suppressWarnings(as.numeric(x)))) {
      return(.kis_cast_like(templ, rep(x, n)))
    }

    x_num <- suppressWarnings(as.numeric(x))
    if (is.na(x_num)) return(.kis_cast_like(templ, rep(NA, n)))

    # the weights come from other datasets (.kis_alloc_weights()), never
    # from the data's own columns
    for (cand in setdiff(cands, "equal")) {
      wname <- .kis_weight_col(cand)
      if (wname %in% names(g)) {
        w_by <- tapply(g[[wname]], g[[obj_id]], function(z) sum(as.numeric(z), na.rm = TRUE))
        w <- as.numeric(w_by[dest_ids])

        if (!all(is.na(w)) && sum(w, na.rm = TRUE) > 0) {
          w <- w / sum(w, na.rm = TRUE)
          return(structure(.kis_cast_like(templ, x_num * w), weight = cand))
        }
      }
    }

    return(structure(.kis_cast_like(templ, rep(x_num / n, n)), weight = "equal"))
  }

  stop(paste("Unknown disaggregation rule:", rule))
}
