#' Combine the harmonized NSI datasets that make up one MD dataset
#'
#' @description
#' An MD dataset can draw on several NSI datasets (e.g. BR from a business
#' register and a survey). Once each is harmonized, this combines them:
#' `"merge"` joins them on the firm id and `year`, keeping all rows of each;
#' `"stack"` appends them, for sources that cover different firms or years.
#'
#' A merge of two sources that share a non-key variable gives `X.x` and
#' `X.y`. With `coalesce_duplicates = TRUE` these become one `X`, taking the
#' first non-missing value, with a warning if the two sources disagree on a
#' row where both have a value.
#'
#' @param dt_list list of data.tables, the harmonized NSI datasets.
#' @param id_vars character, the variables a merge joins on (firm id and
#'   `year`).
#' @param method `"merge"` or `"stack"`.
#' @param weight_var character(1) or `NULL`: a weight variable. On `"stack"`,
#'   each row keeps the weight its own source gave it -- a stack is one
#'   survey split across files (e.g. by year), so each row's weight is still
#'   valid. On `"merge"`, several sources' weights would apply to the same
#'   row at once, which can't be combined, so the variable is dropped from
#'   each source and set to 1 in the result: merged datasets are unweighted.
#' @param coalesce_duplicates logical(1), merge `X.x`/`X.y` pairs into `X`.
#'
#' @return A data.table.
#' @keywords internal
mdi_combine_md_sources <- function(dt_list, id_vars, method = c("merge", "stack"),
                                   weight_var = NULL, coalesce_duplicates = FALSE) {
  if (!is.list(dt_list) || !length(dt_list)) stop("'dt_list' must be a non-empty list of data.tables.")
  check_char_vec(id_vars, "id_vars")
  method <- match.arg(method)
  if (!is.null(weight_var)) check_string(weight_var, "weight_var")

  dt_list <- lapply(dt_list, as.data.table)

  if (method == "stack") {
    # one survey split across files (e.g. by year): each row's own weight
    # is still valid, so it travels with it
    combined <- rbindlist(dt_list, fill = TRUE)
  } else {
    # a merged row draws from several sources at once, whose weights can't
    # be combined into one: dropped from each source, set to 1 below
    if (!is.null(weight_var)) {
      dt_list <- lapply(dt_list, function(dt) {
        if (weight_var %in% names(dt)) dt[, (weight_var) := NULL]
        dt
      })
    }
    combined <- Reduce(function(x, y) merge(x, y, by = id_vars, all = TRUE), dt_list)
    if (coalesce_duplicates) .coalesce_merge_pairs(combined)
    if (!is.null(weight_var)) combined[, (weight_var) := 1]
  }
  combined
}

# X.x / X.y from a merge -> X, first non-missing value.
.coalesce_merge_pairs <- function(dt) {
  for (xc in grep("\\.x$", names(dt), value = TRUE)) {
    stem <- sub("\\.x$", "", xc)
    yc <- paste0(stem, ".y")
    if (!(yc %in% names(dt))) next
    vx <- dt[[xc]]
    vy <- dt[[yc]]
    # fcoalesce needs one type; recode gives factors with source-specific
    # levels, so mixed types go through character (the type conversion after
    # this sets the declared class)
    if (is.factor(vx) || is.factor(vy) || !identical(class(vx), class(vy))) {
      vx <- as.character(vx)
      vy <- as.character(vy)
    }
    n_conflict <- sum(!is.na(vx) & !is.na(vy) & vx != vy)
    if (n_conflict > 0) {
      warning(sprintf("'%s' comes from two sources that disagree on %d row(s); kept the first source's value.",
                      stem, n_conflict), call. = FALSE)
    }
    set(dt, j = stem, value = fcoalesce(vx, vy))
    set(dt, j = c(xc, yc), value = NULL)
  }
  invisible(dt)
}
