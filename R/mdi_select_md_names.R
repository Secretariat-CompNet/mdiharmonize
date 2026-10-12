#' Select the NSI concordance rows and raw variables a run needs
#'
#' @description
#' Two things, both needed before any raw NSI file is imported:
#' - When `cfg$flags$filtered_harmonization` is `TRUE`, narrows the NSI-to-MD
#'   concordance to the MD variables the launch's research modules declare
#'   (`selected`, from [mdi_load_module_selection()]), plus every key
#'   variable of those MD datasets, so only what the modules use gets
#'   harmonized. Selected variables not in `MD_varnames` are reported and
#'   ignored.
#' - Adds a `prepro_vars` list column to `NSI_datafiles`: for each raw file,
#'   the NSI variables its `preprocessing` instructions reference (see
#'   [mdi_preprocess()]), so the import reads them even when no
#'   MD variable maps to them directly.
#'
#' @param cfg A config list built by [mdi_config()], with the MD metadata
#'   (`cfg$md`) and NSI metadata (`cfg$nsi`) loaded.
#' @param selected A `data.table` of the MD variables the modules need
#'   (`MD_dataset`, `MD_varname`), e.g. from [mdi_load_module_selection()].
#'   Required when `cfg$flags$filtered_harmonization` is `TRUE`, ignored
#'   otherwise.
#'
#' @return `cfg`, with `cfg$nsi$NSI_MD_conc` filtered (only when
#'   `cfg$flags$filtered_harmonization` is `TRUE`) and `cfg$nsi$NSI_datafiles`
#'   given the `prepro_vars` column. The tables in the input `cfg` are not
#'   modified.
#'
#' @keywords internal
mdi_select_md_names <- function(cfg, selected = NULL) {
  check_cfg(cfg, need = c("md", "nsi"))
  conc <- cfg$nsi$NSI_MD_conc
  md_varnames <- cfg$md$MD_varnames
  keys <- c("MD_dataset", "MD_varname")

  if (isTRUE(cfg$flags$filtered_harmonization)) {
    if (is.null(selected)) {
      stop("mdi_select_md_names(): filtered harmonization is on, so 'selected' ",
           "(the modules' MD variables, e.g. from mdi_load_module_selection()) is needed.")
    }
    check_dt(selected, keys, "selected")
    selected <- unique(selected[, keys, with = FALSE])

    # every key variable of a selected MD dataset is needed to build it at all
    is_needed_key <- md_varnames[["is_key"]] == 1 &
      md_varnames[["MD_dataset"]] %in% unique(selected[["MD_dataset"]])
    selected <- unique(rbind(selected, md_varnames[is_needed_key, keys, with = FALSE]))

    missing <- selected[!md_varnames, on = keys]
    if (nrow(missing) > 0) {
      message("The following variables are not in MD_varnames and will be ignored:\n",
              paste(utils::capture.output(print(missing)), collapse = "\n"))
      selected <- selected[md_varnames, on = keys, nomatch = 0]
    }

    conc <- merge(selected, conc, by = keys)
  }

  datafiles <- data.table::copy(cfg$nsi$NSI_datafiles)
  datafiles[, ("prepro_vars") := Map(.prepro_needed_vars,
                                     datafiles[["preprocessing"]],
                                     datafiles[["NSI_datafile"]],
                                     MoreArgs = list(nsi_varnames = cfg$nsi$NSI_varnames))]

  cfg$nsi$NSI_MD_conc <- conc
  cfg$nsi$NSI_datafiles <- datafiles
  cfg
}

# The NSI variables a datafile's preprocessing instruction string references
# (the DSL mdi_preprocess() runs), restricted to variables that
# actually exist in that datafile per NSI_varnames.
.prepro_needed_vars <- function(preprocessing_string, datafile, nsi_varnames) {
  all_prepro_vars <- as.character(unlist(lapply(.parse_preprocessing(preprocessing_string), .prepro_step_vars)))
  valid_vars <- nsi_varnames[["NSI_varname"]][which(nsi_varnames[["NSI_datafile"]] == datafile)]
  unique(all_prepro_vars[all_prepro_vars %in% valid_vars])
}

# The columns one parsed preprocessing step reads.
.prepro_step_vars <- function(step) {
  a <- step$args
  switch(step$op,
    agg = c(a$group_cols, names(a$agg_list), names(a$custom_pickmax), unlist(a$custom_pickmax)),
    filter = a$colname,
    dedup = c(a$unique_vars, a$dedupvar),
    parsedate = a$date_col,
    restruct = c(a$var_to_remove, a$unique_vars),
    reshape = c(a$id_vars, a$names_from_vars, a$value_vars),
    # names_to/values_to are created, not read
    reshapelong = c(a$id_vars, a$measure_vars),
    derive = {
      # the variables in the R conditions, plus values and a default that
      # name a column
      values <- c(unname(a$cond_val_pairs), a$default_value)
      c(unlist(lapply(names(a$cond_val_pairs), function(cond) all.vars(str2lang(cond)))),
        values[grepl("^[A-Za-z_][A-Za-z0-9_.]*$", values) & toupper(values) != "NA"])
    },
    scaleif = c(a$condition_col, a$target_cols),
    trimchars = a$target_cols,
    mergefrom = a$join_keys
  )
}
