#' @keywords internal
get_hier <- function(DT, hhfile, hlist_c, hlevel) {
  check_string(hlist_c, "hlist_c")
  if (!is.numeric(hlevel) || length(hlevel) != 1L || hlevel < 1L || hlevel != as.integer(hlevel))
    stop("'hlevel' must be a positive integer.")
  check_dt(DT, required_cols = "node")
  check_dt(hhfile, required_cols = hlist_c, arg_name = "hhfile")
  DTout <- DT[DT[["node"]] == hlist_c]
  hier <- hier_levels(hhfile)
  hlist_p <- hier[which(hier == hlist_c) + hlevel]
  if (is.na(hlist_p))
    stop("'hlevel' (", hlevel, ") exceeds the number of levels above '", hlist_c, "' in hhfile.")
  DTout <- merge(DTout, unique(hhfile[, c(hlist_c, hlist_p), with = FALSE]),
                 by = hlist_c, allow.cartesian = TRUE)
  return(list(DTout, hlist_p))
}

#' Disclosure Check for Aggregated Data
#'
#' @description
#' Applies disclosure control rules to aggregated data based on dominance and
#' minimum-observation criteria. This function is typically called in
#' [`mdi_export()`] for `export_type = "sum_stat"`.
#'
#' Two steps are performed:
#' - **Primary disclosure:**
#'   Suppresses (`-999`) all aggregate cells that fail disclosure rules:
#'   (i) do not satisfy the dominance criterion (`domPerc`), or
#'   (ii) have fewer than `minNumObs` observations.
#'   Flags are recorded as `discflag1_*` and `discflag1`.
#'
#' - **Secondary disclosure:**
#'   If hierarchical aggregation (`hier = TRUE`) is used and exactly one child
#'   cell is suppressed within a parent group, the smallest remaining cell in
#'   that group is also suppressed (`-999`). Flags are recorded as `discflag2`.
#'
#' @param DT A `data.table` containing aggregated statistics and disclosure
#'   variables (e.g. from [`mdi_aggregate()`]). **Must be keyed** on every
#'   grouping/label column (see Details).
#' @param hhfile A `data.table` containing the hierarchy structure, with one
#'   column per aggregation level. Only used when `hier = TRUE`. Default `NULL`.
#' @param hier Logical. Set to `TRUE` if multiple aggregation levels
#'   (child/parent nodes) are present in the dataset. Default: `FALSE`.
#' @param minNumObs Numeric. Minimum number of observations required per cell.
#'   Country-specific; set to `MDIminNumObs` in module/pipeline code.
#'   Default: `5`.
#' @param domSh Numeric. Threshold for maximum allowed dominance share.
#'   Country-specific; set to `MDIdomSh` in module/pipeline code. Default: `0.85`.
#' @param domAllCols Numeric `0`/`1`, country-specific (`MDIdomAllCols` in
#'   `MD_disclosure_info.csv`, defaulting to `0` when absent). Controls how far
#'   the dominance rule reaches:
#'   - `0` (default): `sum`-columns only. Reproduces the historical behaviour.
#'   - `1`: every statistic derived from the dominance variable —
#'     `domPerc_<var>` covers all `<agg>_<var>` columns and a bare `domPerc`
#'     covers every statistic column, so weighted means, medians and quantiles
#'     are protected too.
#' @param dom_direction Character. Direction of the dominance suppression rule.
#'   `"high"` (default) suppresses cells where `domPerc >= domSh` (standard
#'   rule). `"low"` suppresses cells where `domPerc < domSh` (inverted rule,
#'   used for Germany where the dominance formula is defined differently).
#'
#' @return A copy of `DT` (the input is left unchanged), with:
#'   - Suppressed cells (`-999`) where disclosure criteria are not met.
#'   - Flags `discflag1_*` and `discflag1` for primary disclosure.
#'   - Flag `discflag2` for secondary disclosure (if applicable).
#'
#' @details
#' - **The input must be keyed** (`setkeyv`) on all of its grouping/label
#'   columns. The key is the only signal separating label columns from statistic
#'   columns; without it every label would be suppressed to `-999` along with the
#'   numbers. `mdi_aggregate()` sets the key, but `rbindlist()` drops it, so a
#'   table assembled from several aggregations must be re-keyed before export.
#'   Unkeyed input is an error, not silent corruption.
#' - A table carrying `mdi_hier_apply()`'s `node` column must be passed with
#'   `hier = TRUE` (and its `hhfile`); a table with no `node` column must be
#'   passed with `hier = FALSE`. Either mismatch is an error — silently they
#'   would leave a suppressed child recoverable from its parent total, or return
#'   an empty table.
#' - Dominance targets are resolved from the `domPerc` column *name*
#'   (`domPerc_<var>` -> `<agg>_<var>` columns, matched by longest suffix so
#'   `emp` and `tot_emp` do not collide), not paired positionally with
#'   `sum`-columns. A bare `domPerc` (produced when the dominance variable is a
#'   fixed column such as `emp` rather than `"var"`) reaches every `sum`-column.
#' - `dom_direction = "high"` suppresses when `domPerc >= domSh`;
#'   `dom_direction = "low"` suppresses when `domPerc < domSh` (Germany).
#' - Suppression affects both the target aggregate columns and, when triggered,
#'   the `NumObs` column. Secondary disclosure ensures that no single cell can be
#'   back-calculated from its parent total.
#'
#' @examples
#' library(data.table)
#' DT <- data.table(
#'   nace        = c("A", "B", "C"),
#'   sum_emp     = c(100, 200, 40),
#'   mean_emp    = c(5, 8, 2),
#'   domPerc_emp = c(0.50, 0.92, 0.20),
#'   NumObs      = c(10L, 10L, 3L)
#' )
#' setkeyv(DT, "nace")
#' # domAllCols = 0: dominance masks sum_emp only; C is below minNumObs
#' mdi_disclose(DT, hier = FALSE, minNumObs = 5, domSh = 0.85, domAllCols = 0)
#' # domAllCols = 1: dominance also masks mean_emp
#' mdi_disclose(DT, hier = FALSE, minNumObs = 5, domSh = 0.85, domAllCols = 1)
#'
#' @export

mdi_disclose <-
  function(DT,
           hhfile = NULL,
           hier = FALSE,
           minNumObs = 5,
           domSh = 0.85,
           domAllCols = 0,
           dom_direction = c("high", "low")) {

    dom_direction <- match.arg(dom_direction)
    check_dt(DT)
    bygroups <- attr(DT, "sorted")

    # the key is the only signal separating label columns from statistics (see @details)
    if (is.null(bygroups)) {
      stop("mdi_disclose(): the table passed to mdi_export() has no data.table key, ",
           "so label columns cannot be told apart from statistics and would be ",
           "overwritten with -999. Set one before exporting, naming every ",
           "grouping/label column, e.g. setkeyv(DT, c(\"spec\", \"nace\", \"year\")). ",
           "mdi_aggregate() sets the key; rbindlist() drops it again.")
    }
    if (!"NumObs" %in% names(DT)) {
      stop("There's no NumObs column in the table hence the disclosure process ",
           "cannot be done. Check your column names.")
    }
    # masks a copy: the caller's table keeps its values (a disclosure table
    # is an aggregate, so the copy is small)
    DT <- copy(DT)

    cols <-
      names(DT)[which(!(names(DT) %in% c(bygroups, "domPerc", "NumObs", "node")))]
    # anchored: agg type is a PREFIX (<agg>_<var>), so grepl("sum", cols) would
    # also match e.g. mean_sum_assets
    sumcols <- cols[grepl("^sum_", cols)]
    domPercCols <- grep("domPerc", names(DT), value = TRUE)

    # only mdi_hier_apply() stamps a 'node' column, so its presence means the table
    # spans a hierarchy -- a hier/node mismatch fails silently otherwise
    if ("node" %in% names(DT) && !isTRUE(hier) && length(sumcols) != 0) {
      stop("mdi_disclose(): this table has a 'node' column and sum-column(s), but ",
           "mdi_export() was called with hier = FALSE, so secondary disclosure ",
           "did not run. A child cell suppressed by the primary rules can then be ",
           "recovered by subtracting the surviving children from their parent ",
           "total. Pass hier = TRUE, together with the hhfile used to build the ",
           "aggregation.")
    }
    if (isTRUE(hier) && !("node" %in% names(DT))) {
      stop("mdi_disclose(): hier = TRUE but the table has no 'node' column, so ",
           "secondary disclosure has nothing to group on and returns an empty ",
           "table. Pass hier = FALSE, or build the table with mdi_hier_apply().")
    }

    # domPerc columns are inputs to the dominance rule, never a target -- else a
    # domPerc_<var> would mask itself and the flag below would re-read -999
    statcols <- setdiff(cols, domPercCols)

    # owner of a statistic column is the LONGEST variable name it ends with:
    # 'sum_tot_emp' ends with both '_emp' and '_tot_emp' -> belongs to tot_emp
    domVars  <- sub("^domPerc_", "", setdiff(domPercCols, "domPerc"))
    colOwner <- vapply(statcols, function(cl) {
      hit <- domVars[endsWith(cl, paste0("_", domVars))]
      if (length(hit) == 0L) NA_character_ else hit[which.max(nchar(hit))]
    }, character(1))

    # 'domPerc_<var>' -> the statistics of <var>, by name; a bare 'domPerc'
    # -> every statistic. domAllCols == 0 keeps only the sum-columns of those.
    dom_targets <- function(domCol) {
      v <- sub("^domPerc_", "", domCol)
      targets <- if (identical(v, domCol)) statcols else names(colOwner)[colOwner %in% v]
      if (identical(as.numeric(domAllCols), 1)) targets else intersect(targets, sumcols)
    }

    # PRIMARY DISCLOSURE

    for (domCol in domPercCols) {
      targets <- dom_targets(domCol)
      if (length(targets) == 0) next

      # resolved ONCE, before masking -- re-reading the condition for the flag
      # would make it depend on the table still being unmodified
      p   <- DT[[domCol]]
      idx <- if (dom_direction == "low") which(p < domSh) else which(p >= domSh)

      # assigned even when idx is empty, so the column exists on every table
      DT[idx, (paste0("discflag1_", domCol)) := 1]
      for (j in targets) data.table::set(DT, i = idx, j = j, value = -999)
    }

    DT[DT[["NumObs"]] < minNumObs, (cols) := -999]
    DT[DT[["NumObs"]] < minNumObs, ("discflag1") := 1]

    # SECONDARY DISCLOSURE: protects against recovering a suppressed child cell
    # by subtracting the survivors from the parent total
    if (isTRUE(hier) && length(sumcols) != 0) {

      nodes <- sort(unique(DT[["node"]]))
      # from the hierarchy, not sort(nodes): that puts h_10 before h_2
      hlist_top <- tail(hier_levels(hhfile), 1)

      agglevel_c <- bygroups[1]
      agglevel_o <- bygroups[-1]

      DTlist <- lapply(nodes, function(n) {
        if (n != hlist_top) {
          DTagg <- DT[DT[["node"]] == n, ]
          data.table::setnames(DTagg, agglevel_c, n)
          DT_hier <- get_hier(DTagg, hhfile, n, 1L)
          DTagg2 <- DT_hier[[1]]
          agglevel_h <- DT_hier[[2]]

          # disclosed-cell count per parent node / other bygroup
          DTagg2[, ("ndisc") := sum(.SD[["discflag1"]], na.rm = TRUE),
                 by = c(agglevel_h, agglevel_o)]

          # if exactly one child cell in a parent group is already suppressed,
          # also suppress the group's smallest remaining cell (by NumObs)
          DTagg2[, ("rowNr") := seq_len(.N)]
          rowNrs <-
            DTagg2[DTagg2[["ndisc"]] == 1 & is.na(DTagg2[["discflag1"]]), ][
              , .SD[which.min(.SD[["NumObs"]]), ], by = c(agglevel_h, agglevel_o)]$rowNr

          for (j in sumcols) {
            data.table::set(DTagg2, i = rowNrs, j = j, value = -999)
          }
          DTagg2[DTagg2[["rowNr"]] %in% rowNrs, ("discflag2") := 1]
          DTagg2[, (c("ndisc", "rowNr", agglevel_h)) := NULL]

          data.table::setnames(DTagg2, n, agglevel_c)
        } else {
          DTagg2 <- DT[DT[["node"]] == hlist_top, ]
          DTagg2[, ("discflag2") := NA]
        }

        return(DTagg2)
      })
      # use.names = TRUE: child branches gain columns via get_hier()'s merge()
      # (which reorders them), so the top-level branch's column order differs
      # from the children's -- a positional rbindlist() silently swaps values
      # between same-position, different-name columns instead of erroring.
      DTout <- data.table::rbindlist(DTlist, use.names = TRUE)
    } else {
      DTout <- DT
    }

    cols_to_update <- c(names(DTout)[grepl("discflag1", names(DTout))], "NumObs")
    # na.rm = TRUE: a row can carry discflag1 = 1 (min-obs) while another flag
    # column is NA; without na.rm the rowSums is NA and NumObs is left uncensored
    DTout[DTout[, rowSums(.SD == 1, na.rm = TRUE) > 0, .SDcols = grepl("discflag1", names(DTout))],
          (cols_to_update) := -999]

    return(DTout)
  }
