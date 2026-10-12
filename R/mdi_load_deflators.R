#' Load deflators and the NACE-to-deflator-industry concordance
#'
#' @description
#' Reads `auxdata/deflators.csv`, filtered to
#' the country's own rows. Every column except `cc`/`DEFind`/`nace` is
#' coerced to numeric — `deflators.csv` is measurement data (deflator index
#' values), the opposite default from the classification-file loaders
#' (`mdi_read_meta_dir()`/`mdi_load_md_metadata()`/`mdi_load_nsi_metadata()`),
#' so this does not reuse that primitive. Also reads
#' `MD_nace_DEFind_conc.csv`, the concordance mapping NACE codes to the
#' deflator's own industry grouping (`DEFind`).
#'
#' Deliberately country-agnostic: `deflators.csv` carries a real row for
#' every country code that needs one, including variants of a country code
#' used for a second firm-level sample of the same country (their deflators
#' are the country's own -- deflators are national-accounts data, independent
#' of the firm-level sample). No mapping between codes is hardcoded here.
#'
#' @param rocket_dir Character, the `rocket/` directory holding
#'   `auxdata/deflators.csv`.
#' @param mdmeta_dir Character, the launch's `MDmetadata/` directory,
#'   holding `MD_nace_DEFind_conc.csv`.
#' @param country Character, the NSI country code.
#'
#' @return A list with elements `deflators` and `nace_DEFind` (the names
#'   modules know them by as globals).
#' @examples
#' rocket_dir <- paste0(tempfile("rocket_"), "/")
#' mdmeta_dir <- paste0(tempfile("mdmeta_"), "/")
#' dir.create(paste0(rocket_dir, "auxdata"), recursive = TRUE)
#' dir.create(mdmeta_dir)
#' writeLines(c("cc,year,DEFind,pnv", "FI,1995,A,1.1", "PT,1995,A,2.2"),
#'            paste0(rocket_dir, "auxdata/deflators.csv"))
#' writeLines(c("nace,DEFind", "\"0111\",A01"), paste0(mdmeta_dir, "MD_nace_DEFind_conc.csv"))
#' mdi_load_deflators(rocket_dir, mdmeta_dir, "FI")
#' @export
mdi_load_deflators <- function(rocket_dir, mdmeta_dir, country) {
  check_string(rocket_dir, "rocket_dir")
  check_string(mdmeta_dir, "mdmeta_dir")
  check_string(country, "country")

  deflators <- mdi_import_data(rocket_dir, "auxdata/deflators.csv", "csv")
  deflators <- deflators[deflators[["cc"]] == country]
  if (nrow(deflators) == 0) {
    warning("mdi_load_deflators(): auxdata/deflators.csv has no rows for ", country,
            "; modules that deflate will get no deflators.", call. = FALSE)
  }

  vars <- names(deflators)
  numeric_vars <- vars[!vars %in% c("cc", "DEFind", "nace")]
  deflators[, (numeric_vars) := lapply(.SD, as.numeric), .SDcols = numeric_vars]

  nace_DEFind <- mdi_import_data(mdmeta_dir, "MD_nace_DEFind_conc.csv", "csv",
                                  char_columns = c("nace"))

  list(deflators = deflators, nace_DEFind = nace_DEFind)
}
