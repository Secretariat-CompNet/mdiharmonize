#' Add PIM capital to the harmonized SBS
#'
#' @description
#' Adds firm-level capital stocks from the perpetual inventory method
#' ([mdi_build_pim_capital()]) to the harmonized SBS in `cfg$dirs$tmpsave`,
#' if the country has what it needs:
#' \enumerate{
#'   \item a column in `MD_idInfo`;
#'   \item the MD variables it uses mapped in the concordance: BR's firm id and
#'     `nace`; SBS's firm id, `ni_tan` and employment (`persons`, or `emp`
#'     where SBS doesn't map `persons`, e.g. SI);
#'   \item industry capital stocks, `auxdata/capital_stocks_<CC>.csv` in
#'     `cfg$dirs$rocket` (made by `auxdata/generate_capital_stocks.R`);
#'   \item `SBS.RDS` and `BR.RDS` in `cfg$dirs$tmpsave`.
#' }
#' The industry is BR's time-consistent `MDnace`, or its `nace` (with a
#' message) where BR has no `MDnace`.
#' Otherwise it says which is missing and leaves SBS as it is. A BR at
#' another firm level than SBS is first switched to SBS's level, with the
#' country's `<CC>_<finer>_<coarser>_conc.csv` in `cfg$dirs$nsimeta`.
#'
#' @param cfg config list from [mdi_config()], with the MD metadata
#'   (`cfg$md`) and the selected NSI metadata (`cfg$nsi`) loaded.
#'
#' @return `TRUE` if PIM capital was added, `FALSE` if skipped, invisibly.
#' @keywords internal
mdi_add_pim_capital <- function(cfg) {
  check_cfg(cfg, need = c("md", "nsi"))
  country <- cfg$country
  id_info <- cfg$md$MD_idInfo
  conc <- cfg$nsi$NSI_MD_conc
  capital_file <- paste0(cfg$dirs$rocket, "auxdata/capital_stocks_", country, ".csv")

  # (1) ID table has a column for this country
  id_ok <- country %in% names(id_info)

  # (2) the MD variables it uses are mapped
  missing_br <- missing_sbs <- character(0)
  vars_ok <- FALSE
  if (id_ok) {
    id_br <- id_info[[country]][which(id_info$MD_dataset == "BR")]
    id_sbs <- id_info[[country]][which(id_info$MD_dataset == "SBS")]
    br_conc_vars <- conc$MD_varname[which(conc$MD_dataset == "BR")]
    sbs_conc_vars <- conc$MD_varname[which(conc$MD_dataset == "SBS")]
    # initial capital is scaled by employment: persons, or emp where SBS
    # doesn't map persons (e.g. SI)
    emp_var <- if ("persons" %in% sbs_conc_vars) "persons" else "emp"
    missing_br <- setdiff(c(id_br, "nace"), br_conc_vars)
    missing_sbs <- setdiff(c(id_sbs, "ni_tan", emp_var), sbs_conc_vars)
    vars_ok <- length(missing_br) == 0L && length(missing_sbs) == 0L
  }

  # (3) the industry capital stocks
  file_ok <- file.exists(capital_file)

  # (4) SBS and BR harmonized: with MDIdatasets one of them may come from
  # an earlier run, or not exist yet
  rds <- paste0(cfg$dirs$tmpsave, c("SBS.RDS", "BR.RDS"))
  rds_ok <- all(file.exists(rds))

  if (!(id_ok && vars_ok && file_ok && rds_ok)) {
    if (!id_ok) message(country, ": PIM capital skipped \u2014 CountryCode not found in MD_idInfo.")
    if (!vars_ok) {
      message(country, ": PIM capital skipped \u2014 required MD varnames missing: ",
              "BR(", paste(missing_br, collapse = ", "), ") ",
              "SBS(", paste(missing_sbs, collapse = ", "), ").")
    }
    if (!file_ok) {
      message(country, ": PIM capital skipped \u2014 ", basename(capital_file), " not found in rocket/auxdata/.")
    }
    if (!rds_ok) {
      message(country, ": PIM capital skipped \u2014 ", paste(basename(rds[!file.exists(rds)]), collapse = ", "),
              " not in dirTMPSAVE.")
    }
    return(invisible(FALSE))
  }

  sbs <- readRDS(paste0(cfg$dirs$tmpsave, "SBS.RDS"))
  br <- readRDS(paste0(cfg$dirs$tmpsave, "BR.RDS"))
  # a BR at another firm level than SBS is switched to SBS's level first; the
  # concordance file is named finer_coarser
  conctab <- NULL
  if (id_br != id_sbs) {
    id_levels <- .firm_id_levels
    finer <- id_levels[min(match(c(id_br, id_sbs), id_levels))]
    coarser <- id_levels[max(match(c(id_br, id_sbs), id_levels))]
    conctab <- fread(paste0(cfg$dirs$nsimeta, country, "_", finer, "_", coarser, "_conc.csv"))
  }
  # the time-consistent industry, where mdi_concord_nace() made it (always,
  # unless a country ships its own nace -> MDnace table)
  nace_var <- if ("MDnace" %in% names(br)) "MDnace" else "nace"
  if (nace_var == "nace") {
    message(country, ": PIM capital uses BR's nace, as BR has no MDnace (time-consistent industry).")
  }
  sbs <- mdi_build_pim_capital(
    sbs, br,
    capital_stocks = fread(capital_file),
    nace_hier = cfg$md$MD_nace_hier,
    id_sbs = id_sbs, id_br = id_br, conctab = conctab,
    br_varnames = cfg$md$MD_varnames[cfg$md$MD_varnames$MD_dataset == "BR"],
    emp_var = emp_var, nace_var = nace_var
  )
  saveRDS(sbs, paste0(cfg$dirs$tmpsave, "SBS.RDS"))
  invisible(TRUE)
}
