#' Load the MD variables the launch's research modules declare needing
#'
#' @description
#' Reads each module's `<CODE>_MDnames_select.csv` from `progs_dir` and
#' stacks their `MD_dataset`/`MD_varname` columns into one table, the input
#' [mdi_select_md_names()] uses for filtered harmonization.
#'
#' @param progs_dir Character, the folder with the modules' subfolders
#'   (`cfg$dirs$progs`: the launch's `Rmodules/`, or `dirMODULES` when set).
#' @param modules Character vector of 4-letter module codes (`cfg$modules`).
#'
#' @return A `data.table` with columns `MD_dataset` and `MD_varname`, one row
#'   per declared variable (duplicates across modules kept).
#'
#' @keywords internal
mdi_load_module_selection <- function(progs_dir, modules) {
  check_string(progs_dir, "progs_dir")
  files <- paste0(progs_dir, modules, "/", modules, "_MDnames_select.csv")
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    stop("mdi_load_module_selection(): no MDnames_select file for module(s): ",
         paste(basename(dirname(missing)), collapse = ", "))
  }
  data.table::rbindlist(lapply(files, function(f) {
    data.table::fread(f, header = TRUE, select = c("MD_dataset", "MD_varname"))
  }))
}
