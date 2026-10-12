#' Convert a harmonized MD dataset's variables to their declared classes
#'
#' @description
#' Converts every variable of `dt` to the class `md_vars` declares for it, so
#' module code sees the same types in every country, whatever types came out
#' of import and harmonization. Conversions that fail on a value don't error:
#' `as.numeric()`, `as.Date()` etc. turn it into `NA`. The returned log
#' counts those, per variable, and also counts numeric values other than 0/1
#' converted to logical, which `as.logical()` turns into `TRUE` without an
#' `NA` to show for it.
#'
#' Classes: `character`, `integer`, `numeric`, `logical` (from text:
#' `true`/`t`/`1` and `false`/`f`/`0`, anything else `NA`), `date` (with the
#' format in `domain`, if any) and `factor`. `factor` is only applied where
#' `domain` is `MD_codebook`, the variables whose values recode/reclass
#' already checked against the codebook.
#'
#' @param dt data.table, a harmonized MD dataset. Modified by reference.
#' @param dataset character(1), its MD dataset name, for the log.
#' @param md_vars data.table, the variables to convert: `MD_varname`,
#'   `class`, `domain`. Variables not in `dt`, or without a class, are skipped.
#' @param verbose logical(1), print each conversion.
#'
#' @return A list: `data`, `dt` converted; `log`, one row per converted
#'   variable: `MD_dataset`, `MD_varname`, `desired_class`, `na_before`,
#'   `na_after`, `na_introduced`, `logical_out_of_domain`.
#' @examples
#' dt <- data.table::data.table(emp = c("1", "x"), exporter = c(0, 2))
#' vars <- data.table::data.table(MD_varname = c("emp", "exporter"),
#'                                class = c("numeric", "logical"), domain = NA)
#' res <- suppressWarnings(mdi_coerce_md_types(dt, "SBS", vars))
#' res$log
#' @export
mdi_coerce_md_types <- function(dt, dataset, md_vars, verbose = FALSE) {
  check_dt(dt, arg_name = "dt")
  check_string(dataset, "dataset")
  check_dt(md_vars, c("MD_varname", "class", "domain"), "md_vars")

  coerce_fun <- list(
    character = as.character,
    integer   = as.integer,
    numeric   = as.numeric,
    factor    = as.factor,
    logical   = .to_logical
  )

  log <- list()
  for (i in seq_len(nrow(md_vars))) {
    var_name <- md_vars[["MD_varname"]][i]
    if (!(var_name %in% names(dt))) next
    desired_class <- md_vars[["class"]][i]
    desired_class <- tolower(trimws(ifelse(is.na(desired_class), "", desired_class)))
    if (desired_class == "") next
    var_domain <- trimws(ifelse(is.na(md_vars[["domain"]][i]), "", md_vars[["domain"]][i]))

    # as.factor() has no codebook check of its own: only where recode/reclass
    # already validated the levels against MD_codebook
    if (desired_class == "factor" && tolower(var_domain) != "md_codebook") {
      if (verbose) message("Skipping factor coercion for ", var_name,
                           " (domain != MD_codebook, not codebook-validated): ", dataset)
      next
    }

    var_vals <- dt[[var_name]]
    current_class <- class(var_vals)[1]
    if (tolower(current_class) == desired_class) next
    if (verbose) message("Converting ", var_name, " of type ", current_class, " to ", desired_class)

    na_before <- sum(is.na(var_vals))
    # as.logical() turns any non-zero number into TRUE, so a raw 0/1/2 code
    # collapses into TRUE/FALSE with no NA to show for it
    logical_out_of_domain <- 0L
    if (desired_class == "logical" && is.numeric(var_vals)) {
      logical_out_of_domain <- sum(!(var_vals[!is.na(var_vals)] %in% c(0, 1)))
    }

    if (desired_class == "date") {
      set(dt, j = var_name, value = if (var_domain != "") as.Date(as.character(var_vals), format = var_domain)
                                    else as.Date(as.character(var_vals)))
    } else {
      f <- coerce_fun[[desired_class]]
      if (is.null(f)) {
        warning(sprintf("Couldn't convert variable type to '%s' (var=%s, md=%s)",
                        desired_class, var_name, dataset))
        next
      }
      set(dt, j = var_name, value = f(var_vals))
    }

    na_after <- sum(is.na(dt[[var_name]]))
    log[[length(log) + 1L]] <- data.table(
      MD_dataset = dataset, MD_varname = var_name, desired_class = desired_class,
      na_before = na_before, na_after = na_after, na_introduced = na_after - na_before,
      logical_out_of_domain = logical_out_of_domain
    )
  }

  list(data = dt, log = if (length(log)) rbindlist(log) else .empty_fallout_log())
}

.empty_fallout_log <- function() {
  data.table(MD_dataset = character(), MD_varname = character(), desired_class = character(),
             na_before = integer(), na_after = integer(), na_introduced = integer(),
             logical_out_of_domain = integer())
}

# Text values true/t/1 and false/f/0 (any case), anything else NA; numbers
# via as.logical().
.to_logical <- function(x) {
  if (is.logical(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x2 <- trimws(tolower(x))
    out <- rep(NA, length(x2))
    out[x2 %in% c("true", "t", "1")] <- TRUE
    out[x2 %in% c("false", "f", "0")] <- FALSE
    return(out)
  }
  as.logical(x)
}
