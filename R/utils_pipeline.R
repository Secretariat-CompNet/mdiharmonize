# Helpers for the pipeline functions (mdiharmonize). utils.R is mditools' own
# file, kept identical to it.

#' @keywords internal
check_cfg <- function(cfg, need = character(0)) {
  if (!is.list(cfg) || is.null(cfg$dirs) || is.null(cfg$country))
    stop("'cfg' must be a config list built by mdi_config().")
  # the metadata the calling function needs loaded into cfg
  missing <- need[vapply(need, function(n) is.null(cfg[[n]]), logical(1))]
  if (length(missing)) {
    labels <- c(md = "MD", nsi = "NSI")[need]
    stop(deparse(sys.call(-1)[[1]]), "(): 'cfg' needs the ", paste(labels, collapse = " and "),
         " metadata loaded (", paste0("cfg$", need, collapse = ", "), ").", call. = FALSE)
  }
}

# The firm identifier levels, finest first.
.firm_id_levels <- c("plantid", "firmid", "entid", "entgrp")

# TRUE for a single, non-missing, non-empty value (a file's year variable,
# its preprocessing string, ...).
.is_set <- function(x) length(x) == 1L && !is.na(x) && x != ""

# The most frequent non-missing value -- the first to appear, if several are
# equally frequent -- or NA of x's own type if there is none.
.mode <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(x[NA_integer_])
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
