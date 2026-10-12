#' Harmonize a country's NSI data into the MD datasets
#'
#' @description
#' Everything between the raw NSI data and the harmonized MD datasets in
#' `cfg$dirs$tmpsave`:
#' \enumerate{
#'   \item loads the country's NSI metadata ([mdi_load_nsi_metadata()]) and
#'     selects the concordance rows the run needs ([mdi_select_md_names()],
#'     narrowed to the modules' variables with filtered harmonization);
#'   \item orders the NSI datasets so each is read once ([mdi_classify_conc()],
#'     [mdi_conc_order()]);
#'   \item if `cfg$flags$import`: cleans `cfg$dirs$tmpsave` of a previous
#'     run's datasets (if `cfg$flags$clean_tmp`), reads and harmonizes the
#'     data ([mdi_read_firmdata()]), harmonizes classification codes over
#'     time ([mdi_harmonize_class_vars()]) and adds PIM capital to SBS
#'     ([mdi_add_pim_capital()]). Without an import, the run uses the
#'     datasets already in `cfg$dirs$tmpsave`;
#'   \item with `cfg$datasets` (`MDIdatasets`), the import does all this for
#'     the chosen MD datasets only: reads only the NSI files they need,
#'     cleans only their `.RDS` files, replaces only their rows of the
#'     conversion log, harmonizes only their classification codes, and redoes
#'     PIM capital when SBS or BR is chosen. The other datasets in
#'     `cfg$dirs$tmpsave` stay as an earlier run left them;
#'   \item loads the deflators ([mdi_load_deflators()]).
#' }
#'
#' If a data file is in an Oracle database and `cfg$db_user` is set, it asks
#' for the database password (never stored) and closes the connection when
#' done, also on an error, unless `oracle_channel` is given.
#'
#' @param cfg config list from [mdi_config()], with the MD metadata
#'   (`cfg$md`) loaded.
#' @param oracle_channel an open RODBC connection to use instead of asking for
#'   the password; the caller closes it.
#'
#' @return `cfg`, with `nsi` (the selected NSI metadata), `conctype` (the
#'   processing order) and `deflators` added.
#' @keywords internal
mdi_harmonize_pipeline <- function(cfg, oracle_channel = NULL) {
  check_cfg(cfg, need = "md")

  cfg$nsi <- mdi_load_nsi_metadata(cfg$dirs$nsimeta, cfg$dirs$mdmeta, cfg$country)
  message("All NSI Metadata files have been imported successfully")
  selected <- if (isTRUE(cfg$flags$filtered_harmonization)) mdi_load_module_selection(cfg$dirs$progs, cfg$modules)
  cfg <- mdi_select_md_names(cfg, selected)

  cfg$conctype <- .processing_order(cfg$nsi$NSI_MD_conc)

  if (isTRUE(cfg$flags$import)) {
    # the import's own cfg, narrowed to the chosen MD datasets; cfg keeps the
    # whole concordance for the checks and the modules, which read every
    # dataset in tmpsave
    run <- .only_chosen_datasets(cfg)
    run_conctype <- if (is.null(cfg$datasets)) cfg$conctype else .processing_order(run$nsi$NSI_MD_conc)
    # a previous run's MD datasets, before importing anew
    if (isTRUE(cfg$flags$clean_tmp)) {
      old <- dir(cfg$dirs$tmpsave, pattern = "\\.RDS$", full.names = TRUE)
      if (!is.null(cfg$datasets)) old <- old[basename(old) %in% paste0(cfg$datasets, ".RDS")]
      invisible(file.remove(old))
    }
    if (is.null(oracle_channel) && !is.null(cfg$db_user) && any(cfg$nsi$NSI_datafiles$format == "oracle")) {
      oracle_channel <- .oracle_connect(cfg)
      on.exit(.oracle_close(oracle_channel), add = TRUE)
    }
    mdi_read_firmdata(run, run_conctype, oracle_channel)
    mdi_harmonize_class_vars(run)
    # K_pim is SBS's, from BR's industries
    if (is.null(cfg$datasets) || any(c("SBS", "BR") %in% cfg$datasets)) mdi_add_pim_capital(cfg)
  } else {
    if (!is.null(cfg$datasets)) {
      message("MDIdatasets is ignored: with MDIimportFlag = FALSE nothing is harmonized, ",
              "the run uses the MD datasets already in dirTMPSAVE.")
    }
    if (isTRUE(cfg$flags$clean_tmp)) {
      # without an import, those datasets are what this run reads
      warning("MDIcleanTMP = TRUE but MDIimportFlag = FALSE: dirTMPSAVE not cleaned, ",
              "since this run reads its MD datasets from there.")
    }
  }

  cfg$deflators <- mdi_load_deflators(cfg$dirs$rocket, cfg$dirs$mdmeta, cfg$country)
  cfg
}

#' Run MDI up to the modules
#'
#' @description
#' One run at an NSI, up to the research modules: starts
#' `OutputDescription.csv` in `cfg$dirs$output` (launch, rocket and payload
#' versions, then a row per exported file), harmonizes the data
#' ([mdi_harmonize_pipeline()]), writes the post-harmonization report
#' ([mdi_post_harmonization_checks()]) unless the modules are to run
#' (`cfg$flags$module_run`) or the report is switched off
#' (`cfg$flags$phc_run`, e.g. for mock data), and starts
#' `WarningsDescription.txt`.
#'
#' The modules themselves run in the global environment, which a package
#' function doesn't touch: the calling script does that after this.
#'
#' @param cfg config list from [mdi_config()], with the MD metadata
#'   (`cfg$md`) loaded.
#' @param oracle_channel passed on to [mdi_harmonize_pipeline()].
#'
#' @return `cfg`, as [mdi_harmonize_pipeline()] returns it, invisibly.
#' @export
mdi_run <- function(cfg, oracle_channel = NULL) {
  check_cfg(cfg)
  write(
    paste0(
      "This file documents the content of every output file.\nMDI launch version= ",
      cfg$launch_version,
      "\nrocket version = ", cfg$rocket_version$commit_hash,
      "\npayload version = ", cfg$payload_version$commit_hash,
      "\n--------------\n",
      "CountryCode,res_group,output_filename,export_type,MD_varname,bygroups,datasets_used,purpose,share_0_1,zeroes,rel_other_output,selection,description,path"
    ),
    file = paste0(cfg$dirs$output, "OutputDescription.csv"), sep = "\n"
  )

  cfg <- mdi_harmonize_pipeline(cfg, oracle_channel)

  # the review gate: the modules run once someone has looked at the report
  if (!isTRUE(cfg$flags$module_run) && isTRUE(cfg$flags$phc_run)) mdi_post_harmonization_checks(cfg)

  log_warnings("This file documents warnings and additional information.\n--------------\n",
               cfg$dirs$output, append = FALSE)
  invisible(cfg)
}

# Which NSI dataset feeds which MD dataset (1:1, 1:m, m:1, m:m), in the order
# that reads each NSI dataset once.
.processing_order <- function(conc) {
  mdi_conc_order(mdi_classify_conc(conc, left = "NSI_dataset", right = "MD_dataset",
                                   mm_groups = TRUE, mergeconc = FALSE))
}

# cfg with the concordance narrowed to the MD datasets in cfg$datasets (all
# when NULL); stops on a dataset the country doesn't map.
.only_chosen_datasets <- function(cfg) {
  if (is.null(cfg$datasets)) return(cfg)
  conc <- cfg$nsi$NSI_MD_conc
  mapped <- unique(conc[["MD_dataset"]])
  unknown <- setdiff(cfg$datasets, mapped)
  if (length(unknown) > 0) {
    stop("MDIdatasets: ", paste(unknown, collapse = ", "), " not mapped for ", cfg$country,
         if (isTRUE(cfg$flags$filtered_harmonization)) " (with filteredHarmonization, by the modules' variables)",
         ". The MD datasets it maps: ", paste(sort(mapped), collapse = ", "), ".", call. = FALSE)
  }
  cfg$nsi$NSI_MD_conc <- conc[conc[["MD_dataset"]] %in% cfg$datasets]
  cfg
}

# Asks for the Oracle password of cfg$db_user and connects to the DSN of the
# first Oracle data file; the password is never kept.
.oracle_connect <- function(cfg) {
  for (pkg in c("askpass", "RODBC")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Oracle data files need the ", pkg, " package.")
  }
  datafiles <- cfg$nsi$NSI_datafiles
  dsn <- datafiles$path[which(datafiles$format == "oracle")][1]
  RODBC::odbcConnect(dsn = dsn, uid = cfg$db_user,
                     pwd = askpass::askpass(paste0("Enter Oracle password for user ", cfg$db_user,
                                                   " (DSN: ", dsn, "): ")))
}

.oracle_close <- function(channel) RODBC::odbcClose(channel)
