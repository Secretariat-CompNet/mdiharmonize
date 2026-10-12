#' Harmonize classification codes that change over time
#'
#' @description
#' Rewrites the harmonized MD datasets in `cfg$dirs$tmpsave` so a
#' classification's codes mean the same in every year:
#' \itemize{
#'   \item NACE in BR, with [mdi_concord_nace()], unless the country ships its
#'     own NACE classconc (`<CC>_nace_MD_MDnace_classconc.csv`);
#'   \item CN8 in ITGS, PRODCOM codes in PRODCOM and NUTS/LAU in BR, with
#'     [mdi_harmonize_codes_over_time()] and the country's year-on-year
#'     concordance (`<CC>_<code>t0_<code>t1_conc.csv` in
#'     `cfg$dirs$nsimeta`), unless the country ships an MD-level classconc
#'     for that code. Each code table used is exported with [mdi_export()]
#'     (`<code>_classconc`).
#' }
#'
#' @param cfg config list from [mdi_config()], with the MD metadata
#'   (`cfg$md`) and the selected NSI metadata (`cfg$nsi`) loaded.
#'
#' @return `NULL`, invisibly; the datasets are rewritten in place.
#' @keywords internal
mdi_harmonize_class_vars <- function(cfg) {
  check_cfg(cfg, need = c("md", "nsi"))
  tmpsave <- cfg$dirs$tmpsave
  country_dir <- paste0(cfg$dirs$mdmeta, cfg$country, "/")
  conc_datasets <- cfg$nsi$NSI_MD_conc$MD_dataset

  if (!file.exists(paste0(country_dir, cfg$country, "_nace_MD_MDnace_classconc.csv")) &&
      "BR" %in% conc_datasets) {
    message("Running mdi_concord_nace.")
    id_info <- cfg$md$MD_idInfo
    br <- readRDS(paste0(tmpsave, "BR.RDS"))
    br <- mdi_concord_nace(br, id_info[[cfg$country]][which(id_info$MD_dataset == "BR")],
                           weight_emp = FALSE, cumsh_del = 0.95, sh_del = 0.02)
    saveRDS(br, file = paste0(tmpsave, "BR.RDS"))
    message("=> mdi_concord_nace complete.")
  }

  if (!file.exists(paste0(country_dir, cfg$country, "_NSI_cn08_MD_cn08_classconc.csv")) &&
      "ITGS" %in% conc_datasets) {
    message("Running class harmonization for ITGS.")
    .harmonize_class(cfg, "cn08", "ITGS")
  }
  if (!file.exists(paste0(country_dir, cfg$country, "_NSI_pcc8_MD_pcc8_classconc.csv")) &&
      "PRODCOM" %in% conc_datasets) {
    message("Running class harmonization for PRODCOM.")
    .harmonize_class(cfg, "pcc8", "PRODCOM")
  }

  nuts_file_present <- length(
    list.files(cfg$dirs$nsimeta, pattern = "_(nuts([123])t0_nuts\\2t1_conc|laut0_laut1_conc)")
  ) > 0
  if (nuts_file_present && "BR" %in% conc_datasets) {
    message("Running class harmonization for nuts.")
    nuts_levels <- c("nuts3", "nuts2", "nuts1", "lau")
    found <- nuts_levels[nuts_levels %in% unique(cfg$nsi$NSI_MD_conc$MD_varname)]
    if (length(found) == 1) {
      .harmonize_class(cfg, found, "BR")
    } else {
      warning("Can't do nuts harmonization: expected exactly one of ",
              paste(nuts_levels, collapse = "/"), " mapped for BR, found ",
              if (length(found)) paste(found, collapse = ", ") else "none", ".")
    }
  }

  message("==> All class harmonization complete.")
  invisible(NULL)
}

# One code's over-time harmonization: reads the country's year-on-year
# concordance, rewrites the dataset's RDS, exports the code table.
.harmonize_class <- function(cfg, code_name, dataset_name) {
  message("Harmonize class: ", code_name)
  tmpsave <- cfg$dirs$tmpsave

  file_name <- paste0(cfg$dirs$nsimeta, cfg$country, "_", code_name, "t0_", code_name, "t1_conc.csv")
  if (!file.exists(file_name)) {
    warning(paste0("Can't do ", code_name, " harmonization, ", file_name, " was not found."))
    return(invisible())
  }
  conc <- fread(file_name, colClasses = c(left = "character", right = "character", year = "numeric"))

  # years the raw data covers, from the NSI datasets that map this code
  nsi_conc <- cfg$nsi$NSI_MD_conc
  datafiles <- cfg$nsi$NSI_datafiles
  nsi_ref <- unique(nsi_conc$NSI_dataset[nsi_conc$MD_varname == code_name])
  in_ref <- datafiles$NSI_dataset %in% nsi_ref
  data_years <- c(min(datafiles$year_start[in_ref]), max(datafiles$year_end[in_ref]))

  out <- mdi_harmonize_codes_over_time(readRDS(paste0(tmpsave, dataset_name, ".RDS")),
                                       conc, code_name, data_years)
  if (is.null(out)) return(invisible())

  mdi_export(out$codes,
    output_name = paste0(code_name, "_classconc"),
    format = "csv",
    dirOUTPUT = cfg$dirs$output,
    CountryCode = cfg$country,
    res_group = "MD",
    datasets_used = dataset_name,
    purpose = "We need to extract this table to have information about how to map all codes in the classification in question.",
    rel_other_output = paste0("This is the concordance table for ", code_name, " codes."),
    selection = "All firms in the sample",
    export_type = "other",
    share_0_1 = "NA",
    zeroes = "NA",
    description = "A table containing the 1-1 mapping between old codes from the classification to those that have been harmonized to the list available in the final year of the sample."
  )

  saveRDS(out$data, file = paste0(tmpsave, dataset_name, ".RDS"))
  message("Class harmonization ", code_name, " complete.")
}
