#' Order NSI datafile processing to minimise memory use
#'
#' @description
#' Takes the NSI-to-MD dataset relationships classified by [mdi_classify_conc()] and
#' decides the order in which NSI datasets are loaded and MD datasets are
#' built, so that each raw NSI dataset is loaded once and held in memory for
#' as short a time as possible. Adds the columns `ordernum`, `openNSI`,
#' `closeNSI` and `closeMD` that the harmonization loop steps through.
#'
#' @param conc A `data.table` as returned by
#'   `mdi_classify_conc(..., left = "NSI_dataset", right = "MD_dataset", mm_groups = TRUE)`:
#'   one row per NSI-dataset/MD-dataset pair, with columns `NSI_dataset`,
#'   `MD_dataset`, `conctype` (`"1:1"`, `"1:m"`, `"m:1"` or `"m:m"`) and
#'   `compnum`.
#'
#' @return A copy of `conc`, sorted into processing order, with four added
#'   columns: `ordernum` (processing step), `openNSI` (`1` on the first row
#'   that needs an NSI dataset, i.e. load it here), `closeNSI` (`1` on the
#'   last row that needs it, i.e. drop it after), and `closeMD` (`1` on the
#'   last row contributing to an MD dataset, i.e. save it after). `conc`
#'   itself is not modified.
#'
#' @details
#' - `1:1` pairs are numbered `1, 2, ...` in the order they appear in `conc`.
#' - `1:m` pairs get one step per NSI dataset, `m:1` pairs one step per MD
#'   dataset.
#' - Within each `m:m` component (`compnum`), every ordering of its MD
#'   datasets is tried and the one that keeps the fewest unneeded NSI
#'   datasets in memory is used. This search is exhaustive (factorial in the
#'   number of MD datasets per component), which is fine for the component
#'   sizes real concordances produce.
#' - The open/close flags are computed separately within each `conctype`.
#'
#' @keywords internal
mdi_conc_order <- function(conc) {
  check_dt(conc, required_cols = c("NSI_dataset", "MD_dataset", "conctype", "compnum"),
           arg_name = "conc")
  dt <- data.table::copy(conc)

  data.table::set(dt, j = "ordernum", value = 0)

  one_one <- which(dt[["conctype"]] == "1:1")
  data.table::set(dt, i = one_one, j = "ordernum", value = seq_along(one_one))

  counter <- max(dt[["ordernum"]], na.rm = TRUE) + 1
  for (nsi in unique(dt[["NSI_dataset"]][which(dt[["conctype"]] == "1:m")])) {
    rows <- which(dt[["NSI_dataset"]] == nsi & dt[["conctype"]] == "1:m")
    data.table::set(dt, i = rows, j = "ordernum", value = counter)
    counter <- counter + 1
  }
  for (md in unique(dt[["MD_dataset"]][which(dt[["conctype"]] == "m:1")])) {
    rows <- which(dt[["MD_dataset"]] == md & dt[["conctype"]] == "m:1")
    data.table::set(dt, i = rows, j = "ordernum", value = counter)
    counter <- counter + 1
  }

  mm <- dt[which(dt[["conctype"]] == "m:m")]
  if (nrow(mm) > 0) data.table::set(dt, j = "sort_order", value = NA_integer_)
  for (comp in unique(mm[["compnum"]])) {
    comp_rows <- mm[which(mm[["compnum"]] == comp)]
    best <- .best_md_sequence(comp_rows)

    counter <- max(dt[["ordernum"]], na.rm = TRUE) + 1
    processed <- character(0)
    for (md in best) {
      needed <- unique(comp_rows[["NSI_dataset"]][which(comp_rows[["MD_dataset"]] == md)])
      new_nsi <- setdiff(needed, processed)
      if (length(new_nsi) > 0) {
        data.table::set(dt, i = which(dt[["NSI_dataset"]] %in% new_nsi),
                        j = "ordernum", value = counter)
        counter <- counter + 1
      }
      processed <- unique(c(processed, needed))
    }

    for (k in seq_along(best)) {
      rows <- which(dt[["MD_dataset"]] == best[k] & dt[["compnum"]] == comp)
      data.table::set(dt, i = rows, j = "sort_order", value = k)
    }
  }

  if ("sort_order" %in% names(dt)) {
    data.table::setorderv(dt, c("ordernum", "sort_order", "NSI_dataset", "MD_dataset"))
    data.table::set(dt, j = "sort_order", value = NULL)
  } else {
    data.table::setorderv(dt, c("ordernum", "NSI_dataset", "MD_dataset"))
  }

  for (flag in c("openNSI", "closeNSI", "closeMD")) {
    data.table::set(dt, j = flag, value = NA_integer_)
  }
  for (ctype in unique(dt[["conctype"]])) {
    rows <- which(dt[["conctype"]] == ctype)
    nsi <- dt[["NSI_dataset"]][rows]
    md <- dt[["MD_dataset"]][rows]
    data.table::set(dt, i = rows, j = "openNSI", value = as.integer(!duplicated(nsi)))
    data.table::set(dt, i = rows, j = "closeNSI", value = as.integer(!duplicated(nsi, fromLast = TRUE)))
    data.table::set(dt, i = rows, j = "closeMD", value = as.integer(!duplicated(md, fromLast = TRUE)))
  }

  dt[]
}

# Number of NSI datasets held in memory while not needed, summed over the
# steps of building the MD datasets in `sequence` order.
.md_idle_time <- function(sequence, md_to_nsi, nsi_to_md) {
  in_memory <- c()
  idle <- 0
  for (md in sequence) {
    required <- md_to_nsi[md][[1]]
    in_memory <- union(in_memory, setdiff(required, in_memory))
    idle <- idle + length(setdiff(in_memory, required))
    for (nsi in required) {
      if (all(!nsi_to_md[[nsi]] %in% sequence[which(sequence == md):length(sequence)])) {
        in_memory <- setdiff(in_memory, nsi)
      }
    }
  }
  idle
}

.md_permutations <- function(x) {
  if (length(x) == 1) return(list(x))
  out <- list()
  for (i in seq_along(x)) {
    out <- c(out, lapply(.md_permutations(x[-i]), function(rest) c(x[i], rest)))
  }
  out
}

# The MD-dataset build order with the least idle time within one m:m component.
.best_md_sequence <- function(comp_rows) {
  md_to_nsi <- split(comp_rows[["NSI_dataset"]], comp_rows[["MD_dataset"]])
  nsi_to_md <- split(comp_rows[["MD_dataset"]], comp_rows[["NSI_dataset"]])
  best <- NULL
  best_idle <- Inf
  for (s in .md_permutations(unique(comp_rows[["MD_dataset"]]))) {
    idle <- .md_idle_time(s, md_to_nsi, nsi_to_md)
    if (idle < best_idle) {
      best_idle <- idle
      best <- s
    }
  }
  best
}
