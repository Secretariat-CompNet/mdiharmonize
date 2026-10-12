#' The column holding a given level of a hierarchy file
#'
#' @description
#' Hierarchy files (`MD_nace_hier.csv`, `MD_nuts_hier.csv`, `MD_kic_hier.csv`)
#' name their columns `h_0`, `h_1`, ... — the number is the level index, finest
#' first. Nothing about what a level *means* is encoded in that name;
#' `MD_hier_levels.csv` declares it. `hier_col()` answers "which column holds
#' NUTS 3", so module code asks for the level it wants instead of hard-coding
#' `h_1`.
#'
#' @param hier Character, the hierarchy's name as it appears in
#'   `MD_hier_levels.csv`'s `hier` column and as the loaded object is named —
#'   e.g. `"MD_nuts_hier"`.
#' @param hlevels The declaration table, `MD_hier_levels.csv` as loaded: in a
#'   module, `MD_hier_levels`.
#' @param level_name Character, the level's stable short name — `"nuts2"`,
#'   `"division2"`, `"total"`. Give exactly one of `level_name` or `md_varname`.
#' @param md_varname Character, an MD variable whose codes sit at the wanted
#'   level — `"nuts3"`. Use this when the level is decided by which variable the
#'   data happens to carry.
#'
#' @return One column name, e.g. `"h_1"`.
#'
#' @seealso [hier_levels()] for a file's columns in level order.
#'
#' @examples
#' # in a run, MD_hier_levels is the loaded metadata file
#' levels <- data.table::data.table(
#'   hier = "MD_nuts_hier", level = c("h_0", "h_1", "h_2"),
#'   level_name = c("lau", "nuts3", "nuts2"), md_varname = c("lau", "nuts3", "nuts2"))
#' hier_col("MD_nuts_hier", level_name = "nuts2", hlevels = levels)   # "h_2"
#' hier_col("MD_nuts_hier", md_varname = "nuts3", hlevels = levels)   # "h_1"
#'
#' @export
hier_col <- function(hier, hlevels, level_name = NULL, md_varname = NULL) {
  if (missing(hlevels)) {
    stop("hier_col(): 'hlevels' is required, the loaded MD_hier_levels.csv ",
         "(in a module: hlevels = MD_hier_levels).")
  }
  check_dt(hlevels, c("hier", "level", "level_name", "md_varname"), "hlevels")
  if (is.null(level_name) == is.null(md_varname)) {
    stop("hier_col(): give exactly one of level_name or md_varname.")
  }
  # Plain vectors, not data.table `[`: every argument here shares a name with a
  # column of MD_hier_levels, and inside `[.data.table` the column would win.
  h_col  <- as.character(hlevels[["hier"]])
  lvl    <- as.character(hlevels[["level"]])
  lname  <- as.character(hlevels[["level_name"]])
  mdvar  <- as.character(hlevels[["md_varname"]])

  in_hier <- which(h_col == hier)
  if (length(in_hier) == 0L) {
    stop("hier_col(): no hierarchy '", hier, "' declared in MD_hier_levels.csv. ",
         "Declared: ", paste(unique(h_col), collapse = ", "))
  }

  hit <- if (!is.null(level_name)) {
    in_hier[!is.na(lname[in_hier]) & lname[in_hier] == level_name]
  } else {
    in_hier[!is.na(mdvar[in_hier]) & nzchar(mdvar[in_hier]) &
              mdvar[in_hier] == md_varname]
  }

  if (length(hit) != 1L) {
    wanted <- if (is.null(level_name)) md_varname else level_name
    field  <- if (is.null(level_name)) "md_varname" else "level_name"
    stop("hier_col(): ", field, " '", wanted, "' matches ", length(hit),
         " level(s) of ", hier, ". Levels declared: ",
         paste(lname[in_hier], collapse = ", "), "; MD variables: ",
         paste(mdvar[in_hier][!is.na(mdvar[in_hier]) & nzchar(mdvar[in_hier])],
               collapse = ", "), ".")
  }
  lvl[hit]
}
