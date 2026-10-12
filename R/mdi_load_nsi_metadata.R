#' Load NSI metadata for one country
#'
#' @description
#' Reads the country's stacked NSI files
#' (`_datafiles.csv`, `_varnames.csv`) from `nsimeta_dir`, and the
#' NSI-to-MD concordance/classification files (`_conc`, `_classconc`,
#' `_codeconc`, plus the NSI-level and MD-level `_class`/`_classconc`
#' family) from `nsimeta_dir` and `mdmeta_dir/<country>/`, via
#' `mdi_read_meta_dir()`, renamed from their `<country>_...` filenames to
#' `NSI_...`-prefixed names. Per-classification files are included (e.g.
#' `FI_cn08_class.csv` -> `NSI_cn08_class`,
#' `FI_NSI_cn08_MD_cn08_classconc.csv` -> `NSI_NSI_cn08_MD_cn08_classconc`):
#' `mdi_harmonize()`'s `reclass` step looks them up by exactly these names.
#' `NSI_datafiles.csv` is read plainly (it has
#' `year_start`/`year_end` numeric columns that must not be forced to
#' character). Its `path` column is relative to the input data folder as
#' given (for mock data with one subfolder per country, that folder is the
#' country's).
#'
#' @param nsimeta_dir Character, the country's `rocket/NSImetadata/<CC>/`
#'   directory.
#' @param mdmeta_dir Character, the launch's `MDmetadata/` directory — the country's `NSI_MD_conc`-family files live under
#'   `mdmeta_dir/<country>/`.
#' @param country Character, the NSI country code.
#'
#' @return A named list of `data.table`s: `NSI_datafiles`, `NSI_varnames`,
#'   plus every matched NSI-MD concordance/classification file (e.g.
#'   `FI_MD_conc.csv` -> `NSI_MD_conc`, `FI_classconc.csv` ->
#'   `NSI_classconc`).
#' @examples
#' nsimeta_dir <- paste0(tempfile("nsimeta_"), "/")
#' mdmeta_dir <- paste0(tempfile("mdmeta_"), "/")
#' dir.create(nsimeta_dir)
#' dir.create(paste0(mdmeta_dir, "FI"), recursive = TRUE)
#' writeLines(c("NSI_datafile,path,year_start,year_end", "bd2003,bd/,2003,2003"),
#'            paste0(nsimeta_dir, "FI_datafiles.csv"))
#' writeLines(c("NSI_datafile,NSI_varname,is_key", "bd2003,firmid,1"),
#'            paste0(nsimeta_dir, "FI_varnames.csv"))
#' writeLines(c("NSI_dataset,NSI_varname,MD_varname,method,detail,year",
#'              "bd,acq,acq,revalue,as.logical(x),2003"),
#'            paste0(mdmeta_dir, "FI/FI_MD_conc.csv"))
#' nsi <- mdi_load_nsi_metadata(nsimeta_dir, mdmeta_dir, "FI")
#' names(nsi)
#' @export
mdi_load_nsi_metadata <- function(nsimeta_dir, mdmeta_dir, country) {
  check_string(nsimeta_dir, "nsimeta_dir")
  check_string(mdmeta_dir, "mdmeta_dir")
  check_string(country, "country")

  rename_nsi <- function(x) {
    names(x) <- sub(paste0("^", country), "NSI", names(x))
    x
  }

  result <- list()

  dt <- mdi_read_plain_csv(paste0(nsimeta_dir, country, "_datafiles.csv"))
  if (!is.null(dt)) result$NSI_datafiles <- dt

  # NSI-level files: varnames, plus every classification and classconc file,
  # including the per-classification ones mdi_harmonize()'s reclass step
  # looks up by name, e.g. FI_cn08_class.csv -> NSI_cn08_class
  nsi_level <- rename_nsi(mdi_read_meta_dir(
    nsimeta_dir,
    pattern = paste0("^", country, "_(varnames|(.+_)?class|(.+_)?classconc)\\.csv$")
  ))

  # The per-country concordance folder: NSI-to-MD conc/codeconc, plus every
  # classconc file, e.g. FI_MD_classconc.csv, and
  # FI_NSI_cn08_MD_cn08_classconc.csv -> NSI_NSI_cn08_MD_cn08_classconc
  md_level <- rename_nsi(mdi_read_meta_dir(
    paste0(mdmeta_dir, country, "/"),
    pattern = paste0("^", country, "_(MD_conc|MD_codeconc|(.+_)?classconc)\\.csv$")
  ))

  c(result, nsi_level, md_level)
}
