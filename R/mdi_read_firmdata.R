#' Read and harmonize a country's NSI data into MD datasets
#'
#' @description
#' Builds every MD dataset of the run and saves each to
#' `cfg$dirs$tmpsave` as `<MD_dataset>.RDS`, the MD virtual datasets the
#' modules read. For each MD dataset, in the order of `conctype`:
#' \enumerate{
#'   \item reads its NSI datasets' files ([mdi_read_nsi_datafile()]), unless
#'     an earlier MD dataset already did;
#'   \item harmonizes each file ([mdi_harmonize()]) and stacks the files of
#'     each NSI dataset;
#'   \item combines the NSI datasets ([mdi_combine_md_sources()]): stacked
#'     for FR, DE and AT's ITGS, merged on firm id and `year` otherwise;
#'   \item sorts by its key variables and converts every variable to its
#'     declared class ([mdi_coerce_md_types()]).
#' }
#' NSI files stay in memory only while a later MD dataset still needs them,
#' and only with the columns it needs (`openNSI`/`closeNSI` in `conctype`,
#' from [mdi_conc_order()]).
#'
#' The conversion log (NAs introduced, out-of-domain logicals) is saved as
#' `coercion_fallout_log.csv` in `cfg$dirs$tmpsave`, for the
#' post-harmonization checks. When only some MD datasets are harmonized
#' (`cfg$datasets`), only their rows of an existing log are replaced.
#'
#' @param cfg config list from [mdi_config()], with the MD metadata
#'   (`cfg$md`) and the selected NSI metadata (`cfg$nsi`, see
#'   [mdi_select_md_names()]) loaded.
#' @param conctype data.table, the processing order: `NSI_dataset`,
#'   `MD_dataset`, `openNSI`, `closeNSI` ([mdi_conc_order()]).
#' @param oracle_channel an open RODBC connection, for NSI files with
#'   `format == "oracle"`.
#'
#' @return The conversion log, invisibly.
#' @keywords internal
mdi_read_firmdata <- function(cfg, conctype, oracle_channel = NULL) {
  check_cfg(cfg, need = c("md", "nsi"))
  check_dt(conctype, c("NSI_dataset", "MD_dataset", "openNSI", "closeNSI"), "conctype")

  conc <- cfg$nsi$NSI_MD_conc
  datafiles <- cfg$nsi$NSI_datafiles
  code_tables <- c(cfg$nsi, cfg$md)
  proc_order <- unique(conctype[["MD_dataset"]])
  # NSI files read and still needed, by file name
  loaded <- list()
  fallout <- list()

  for (md in proc_order) {
    message("Starting harmonization of ", md)
    # the selection outside [ ]: conctype has a column named conctype
    in_md <- conctype[["MD_dataset"]] == md
    md_rows <- conctype[in_md]
    conc_md <- conc[conc[["MD_dataset"]] == md]
    sources <- list()

    for (nsi in md_rows[["NSI_dataset"]]) {
      nsi_row <- md_rows[md_rows[["NSI_dataset"]] == nsi]
      file_rows <- datafiles[datafiles[["NSI_dataset"]] == nsi]

      if (nsi_row[["openNSI"]] == 1) {
        for (i in seq_len(nrow(file_rows))) {
          loaded[[file_rows[["NSI_datafile"]][i]]] <- mdi_read_nsi_datafile(cfg, file_rows[i], oracle_channel)
        }
      }

      files <- unique(file_rows[, c("NSI_datafile", "yearvar", "year_start")])
      harmonized <- vector("list", nrow(files))
      for (i in seq_len(nrow(files))) {
        datafile <- files[["NSI_datafile"]][i]
        year_var <- files[["yearvar"]][i]
        conc_file <- conc_md[which(conc_md[["NSI_dataset"]] == nsi & conc_md[["year"]] == files[["year_start"]][i])]
        vars <- unique(conc_file[["NSI_varname"]])
        if (.is_set(year_var)) vars <- unique(c(year_var, vars))
        missing <- setdiff(vars, names(loaded[[datafile]]))
        if (length(missing)) {
          stop("NSI datafile '", datafile, "' (", nsi, ", ", files[["year_start"]][i],
               ") lacks variable(s) ", paste(missing, collapse = ", "), " that the concordance maps. ",
               "Check that ", cfg$country, "_varnames.csv lists them for this file, that the file has ",
               "them, and that its preprocessing doesn't drop them.",
               call. = FALSE)
        }
        harmonized[[i]] <- mdi_harmonize(loaded[[datafile]][, vars, with = FALSE], conc_file, cfg$country,
                                         code_tables = code_tables, year_var = year_var)
        message("===> Datafile ", datafile, " harmonized!")
      }
      # drop or trim the raw files before stacking, and the pieces right after:
      # raw files, pieces and stacked table all in memory at once raise the
      # peak memory by several GB on large data
      loaded <- .keep_for_later(loaded, files, nsi, nsi_row[["closeNSI"]] == 1, md, proc_order,
                                conctype, conc)
      sources[[nsi]] <- rbindlist(harmonized, fill = TRUE)
      rm(harmonized)
    }

    dt <- if (length(sources) > 1) {
      has_weights <- any(grepl("weights_", conc_md[["MD_varname"]]))
      mdi_combine_md_sources(
        unname(sources),
        id_vars = c(cfg$md$MD_idInfo[[cfg$country]][cfg$md$MD_idInfo[["MD_dataset"]] == md], "year"),
        method = .stack_or_merge(cfg$country, md),
        weight_var = if (has_weights) paste0("weights_", tolower(md), "_nsi"),
        # sources sharing a non-key variable give X.x/X.y, which later steps
        # don't expect
        coalesce_duplicates = TRUE
      )
    } else {
      sources[[1]]
    }
    rm(sources)

    setkeyv(dt, .md_sort_keys(dt, cfg$md$MD_varnames, md, cfg$country))

    # every MD variable to its declared class, logging the NAs that
    # introduces
    md_vars <- cfg$md$MD_varnames[unique(conc[, c("MD_dataset", "MD_varname")]),
                                  on = c("MD_dataset", "MD_varname"), nomatch = 0]
    md_vars <- unique(md_vars[md_vars[["MD_dataset"]] == md, c("MD_varname", "class", "domain")])
    message("Converting variables to MD_varnames types")
    coerced <- mdi_coerce_md_types(dt, md, md_vars, verbose = isTRUE(cfg$flags$debug))
    fallout[[md]] <- coerced$log

    saveRDS(coerced$data, file = paste0(cfg$dirs$tmpsave, md, ".RDS"))
    message("======> MD_dataset ", md, " fully harmonized! <======")
    rm(dt, coerced)
  }

  # rbind of a list with the typed empty log first keeps the columns' types
  # when no variable needed converting
  log_file <- paste0(cfg$dirs$tmpsave, "coercion_fallout_log.csv")
  earlier <- NULL
  if (!is.null(cfg$datasets) && file.exists(log_file)) {
    # the other datasets' rows, from the run that made them
    earlier <- fread(log_file, colClasses = vapply(.empty_fallout_log(), class, character(1)))
    earlier <- earlier[!earlier[["MD_dataset"]] %in% proc_order]
  }
  fallout <- rbindlist(c(list(.empty_fallout_log(), earlier), unname(fallout)))
  fwrite(fallout, file = log_file)
  invisible(fallout)
}

# Which countries' sources are stacked rather than merged (the others: merged
# on firm id and year).
.stack_or_merge <- function(country, md) {
  if (country %in% c("FR", "DE") || (country == "AT" && md == "ITGS")) "stack" else "merge"
}

# After an NSI dataset's files are harmonized for one MD dataset: drop them
# if no later MD dataset needs them (closeNSI), else keep only the columns
# the later ones need. The later ones are those after `md` in the processing
# order, not the rows after it: a dataset's sources can span several
# ordernum groups (e.g. SBS from sources A and B, where A also feeds BD, built
# in between).
.keep_for_later <- function(loaded, files, nsi, close, md, proc_order, conctype, conc) {
  if (close) {
    loaded[files[["NSI_datafile"]]] <- NULL
    return(loaded)
  }
  later_md <- proc_order[seq_along(proc_order) > match(md, proc_order)]
  later_md <- unique(conctype[["MD_dataset"]][conctype[["MD_dataset"]] %in% later_md &
                                               conctype[["NSI_dataset"]] == nsi])
  for (i in seq_len(nrow(files))) {
    datafile <- files[["NSI_datafile"]][i]
    if (is.null(loaded[[datafile]])) next
    keep <- unique(conc[["NSI_varname"]][which(conc[["NSI_dataset"]] == nsi & conc[["MD_dataset"]] %in% later_md &
                                                 conc[["year"]] == files[["year_start"]][i])])
    year_var <- files[["yearvar"]][i]
    if (.is_set(year_var)) keep <- unique(c(keep, year_var))
    # a variable the file lacks is reported when the later dataset is built,
    # with the file's name
    loaded[[datafile]] <- loaded[[datafile]][, intersect(keep, names(loaded[[datafile]])), with = FALSE]
  }
  loaded
}

# Sort keys of an MD dataset: the firm id(s) in the data, then its other key
# variables that the country maps.
.md_sort_keys <- function(dt, md_varnames, md, country) {
  firm_ids <- .firm_id_levels
  keys <- md_varnames[["MD_varname"]][which(md_varnames[["MD_dataset"]] == md & md_varnames[["is_key"]] == 1)]
  keys <- c(firm_ids[firm_ids %in% names(dt)], keys[!keys %in% firm_ids])
  missing <- setdiff(keys, names(dt))
  if (length(missing) > 0) {
    message("MD dataset '", md, "': key variable(s) not mapped for ", country,
            ", skipped for sorting: ", paste(missing, collapse = ", "))
  }
  intersect(keys, names(dt))
}
