#' Build the disclosure-control configuration for one country
#'
#' @description
#' Reads `MD_disclosure_info.csv`'s `disclosure_variable` column and the
#' country's own column, and returns them as a named list. Every value is
#' a number, except `MDIdomVar` (a variable name) and `MDIdomDirection`
#' (`"high"` or `"low"`), kept as text.
#'
#' @param mdmeta_dir Character, the launch's `MDmetadata/` directory.
#' @param country Character, the NSI country code.
#'
#' @return A named list, one element per row of `MD_disclosure_info.csv`,
#'   under the file's own `disclosure_variable` names verbatim (currently
#'   `MDIminNumObs`, `MDIdomVar`, `MDIdomSh`, `MDIdomNr`, `MDIdomAllCols`,
#'   `MDIdomDirection`, `show_domPerc`, `show_values`). Numbers, except
#'   `MDIdomVar` and `MDIdomDirection` (`"high"`, or `"low"` for DE).
#'
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#' writeLines(c(
#'   "disclosure_variable,FI",
#'   "MDIminNumObs,3",
#'   "MDIdomVar,var"
#' ), paste0(dir, "/MD_disclosure_info.csv"))
#' mdi_disclosure_config(paste0(dir, "/"), "FI")
#' @export
mdi_disclosure_config <- function(mdmeta_dir, country) {
  check_string(mdmeta_dir, "mdmeta_dir")
  check_string(country, "country")

  info <- data.table::fread(paste0(mdmeta_dir, "MD_disclosure_info.csv"))
  if (!country %in% names(info)) {
    stop("mdi_disclosure_config(): MD_disclosure_info.csv has no column for ", country,
         ". Countries in it: ", paste(setdiff(names(info), "disclosure_variable"), collapse = ", "), ".",
         call. = FALSE)
  }
  info <- info[, .SD, .SDcols = c("disclosure_variable", country)]

  # every setting is a number, except MDIdomVar (a variable name) and
  # MDIdomDirection ("high", or "low" for DE's inverted rule)
  text_settings <- c("MDIdomVar", "MDIdomDirection")
  result <- Map(function(var_name, var_value) {
    if (var_name %in% text_settings) as.character(var_value) else as.double(var_value)
  }, info[["disclosure_variable"]], info[[country]])
  names(result) <- info[["disclosure_variable"]]
  result
}
