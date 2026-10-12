#' Load MD metadata for the current launch
#'
#' @description
#' Reads every MD classification/lookup file
#' (`_codebook`, `_varnames`, `_catalogue`, `_idInfo`, `_hier_levels`,
#' `_classconc`, `_hier`, `_class`, `_indentedclass`) via
#' `mdi_read_meta_dir()`. `MD_datafiles.csv` is read separately, plainly (no
#' column of it needs character coercion, and the equivalent NSI-side file
#' has numeric `year_start`/`year_end` columns that must not be forced to
#' character, so both are kept off the shared rule for consistency).
#' `MD_disclosure_info.csv` is not included here — see
#' `mdi_disclosure_config()`.
#'
#' @param mdmeta_dir Character, the launch's `MDmetadata/` directory.
#'
#' @return A named list of `data.table`s, one per matched file, named after
#'   the file's own basename without extension (e.g. `MD_nace_hier.csv` ->
#'   `MD_nace_hier`), plus `MD_datafiles` if present.
#'
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#' writeLines(c("code,description", "1,Active"), paste0(dir, "/MD_codebook.csv"))
#' writeLines(c("name,description"), paste0(dir, "/MD_datafiles.csv"))
#' md <- mdi_load_md_metadata(paste0(dir, "/"))
#' names(md)
#' @export
mdi_load_md_metadata <- function(mdmeta_dir) {
  check_string(mdmeta_dir, "mdmeta_dir")

  suffixes <- c("_codebook", "_varnames", "_catalogue", "_idInfo",
                "_hier_levels", "_classconc", "_hier", "_class",
                "_indentedclass")
  pattern <- paste0("(", paste(suffixes, collapse = "|"), ")\\.csv$")
  md <- mdi_read_meta_dir(mdmeta_dir, pattern = pattern)

  dt <- mdi_read_plain_csv(paste0(mdmeta_dir, "MD_datafiles.csv"))
  if (!is.null(dt)) md$MD_datafiles <- dt

  md
}
