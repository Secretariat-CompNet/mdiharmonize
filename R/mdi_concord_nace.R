#' NACE classification harmonization
#'
#' @description
#' This tool harmonizes NSI NACE classification over time.
#' It simplify the harmonization process using the mode NACE code in the post
#' break year as MD NACE code, and harmonizing accordingly previous year codes.
#' It defines the break year as the year with the most changes in NACE codes.
#'
#' @param DT the MD dataset (e.g. `BR`) to harmonize, already loaded (was
#'        loaded internally from `dirTMPSAVE` given a `dataset_name`).
#' @param id_col character, the name of `DT`'s row-identifier column for
#'        this country (e.g. `entid`, `plantid`) -- was looked up
#'        internally from `MD_idInfo`/`CountryCode` given a `dataset_name`.
#' @param weight_emp logical to decide whether or not to weight code matches
#'        by employment (instead of number of firms).
#' @param cumsh_del number indicating the cumulative residual share of firms
#'        deleted for the construction of year by year concordances.
#' @param sh_del number indicating the share of firms deleted for the
#'        construction of year by year concordances.
#' @param weight_col character, the name of `DT`'s employment-weight column,
#'        used only when `weight_emp` is `TRUE`.
#' @param seed integer, the random seed for the firms whose code is assigned
#'        by a random draw (weighted by how other firms' codes moved), so a
#'        rerun gives the same `MDnace`. The caller's random state is restored
#'        afterwards. `NULL` uses the current random state instead.
#'
#' @return the original MD dataset with the old NSI NACE code and the new MD NACE.
#'
#'
#' @examples
#' library(data.table)
#' # four firms whose NACE codes change with the 2008 revision
#' BR <- data.table(
#'   entid = rep(c("A", "B", "C", "D"), each = 4),
#'   year  = rep(2006:2009, times = 4),
#'   nace  = c("1511", "1511", "1011", "1011",
#'             "1511", "1511", "1011", "1011",
#'             "1513", "1513", "1013", "1013",
#'             "5211", "5211", "4711", "4711")
#' )
#' br_harmonized <- mdi_concord_nace(BR, id_col = "entid", weight_emp = FALSE,
#'                                   cumsh_del = 0.95, sh_del = 0.02)
#' br_harmonized[entid == "D"]  # pre-2008 "5211" mapped to "4711"
#'
#' @export
mdi_concord_nace <- function(DT, id_col, weight_emp, cumsh_del, sh_del, weight_col = "persons_br",
                             seed = 1L) {
  if (!is.null(seed)) .with_local_seed(seed)

  # Get the dataset's key id (e.g., entid, plantid, etc.)
  key_id <- id_col
  if (is.na(key_id) || key_id == "") stop("Missing firm identifier: 'id_col' is NA or empty.")
  missing <- setdiff(c(key_id, "nace", "year", if (weight_emp) weight_col), names(DT))
  if (length(missing)) stop("mdi_concord_nace(): column(s) ", paste(missing, collapse = ", "), " not in the data.")

  # the result is DT with MDnace added; the working copy holds only the
  # columns the concordance needs (BR can be tens of millions of rows)
  MDDT <- copy(DT)
  DT <- MDDT[, c(key_id, "nace", "year", if (weight_emp) weight_col), with = FALSE]
  set(DT, j = "wtvar", value = if (weight_emp) DT[[weight_col]] else 1)
  DT <- DT[, c(key_id, "nace", "year", "wtvar"), with = FALSE]

  # Rename key column to idvar
  setnames(DT, key_id, "idvar")
  setkeyv(DT, c("idvar", "year"))

  # Fill in missing years per firm
  span <- DT[, list(lo = min(.SD[["year"]]), hi = max(.SD[["year"]])), by = "idvar", .SDcols = "year"]
  n_years <- span[["hi"]] - span[["lo"]] + 1L
  all_years <- data.table(idvar = rep(span[["idvar"]], n_years),
                          year = sequence(n_years, from = span[["lo"]]))
  DT <- merge(all_years, DT, by = c("idvar", "year"), all.x = TRUE)
  # a missing year takes the firm's previous code (or, before its first, the
  # next one) and its previous weight
  set(DT, j = "nace", value = .fill_in_group(DT[["nace"]], DT[["idvar"]], "locf"))
  set(DT, j = "nace", value = .fill_in_group(DT[["nace"]], DT[["idvar"]], "nocb"))
  set(DT, j = "wtvar", value = .fill_in_group(DT[["wtvar"]], DT[["idvar"]], "locf"))

  # Previous year's Nace code
  set(DT, j = "l1nace", value = .lag_in_group(DT[["nace"]], DT[["idvar"]]))

  # Detect break year: the year with most firms changing NACE code (the
  # latest, if several)
  change_years <- DT[["year"]][which(DT[["l1nace"]] != DT[["nace"]])]
  break_year <- NA
  if (length(change_years)) {
    n_changes <- table(change_years)
    break_year <- max(as.numeric(names(n_changes)[n_changes == max(n_changes)]))
  }

  if (is.na(break_year)) {
    set(MDDT, j = "MDnace", value = MDDT[["nace"]])
    return(MDDT)
  }
  set(DT, j = "post_break", value = DT[["year"]] >= break_year)

  # Detect mode in pre and post break period: the most frequent code, the
  # latest if several
  # (counted over the whole table at once: per group, this was most of the run)
  period <- c("idvar", "post_break")
  counts <- DT[, list(N = .N, last_year = max(.SD[["year"]])), by = c(period, "nace"), .SDcols = "year"]
  setorderv(counts, c(period, "N", "last_year"), c(1, 1, -1, -1))
  modes <- unique(counts, by = period)
  DT[modes, on = period, ("mode_nace") := mget("i.nace")]
  set(DT, j = "l1mode_nace", value = .lag_in_group(DT[["mode_nace"]], DT[["idvar"]]))

  # Different treatment depending on whether firms at the break, before, or after
  DTc <- DT[DT[["year"]] == break_year & !is.na(DT[["l1nace"]]), "idvar", with = FALSE]
  DT1 <- DT[DT[["year"]] >= break_year][!DTc, on = "idvar"]
  DT0 <- DT[DT[["year"]] < break_year][!DTc, on = "idvar"]
  DTc <- DT[DTc, on = "idvar"]

  # Concordance between pre and post break Nace codes
  conc <- DTc[DTc[["year"]] == break_year, list(wtvar = sum(.SD[["wtvar"]], na.rm = TRUE)),
              by = c("l1mode_nace", "mode_nace"), .SDcols = "wtvar"]
  conc <- .nace_conc_types(conc)
  setorderv(conc, c("conctype", "wtvar"), c(1, -1))
  conc[, ("cum_wtvar") := cumsum(.SD[["wtvar"]]), by = "conctype", .SDcols = "wtvar"]

  # Deletion rule for the concordance
  conc <- conc[!(conc[["cum_wtvar"]] > cumsh_del & conc[["wtvar"]] < sh_del)]

  # Types of concordance between Nace codes (1:1, 1:m, m:1, or m:m)
  conc <- .nace_conc_types(conc[, c("l1mode_nace", "mode_nace", "wtvar"), with = FALSE])

  # For firms only before the break, harmonization based on concordances, if available
  in_conc <- function(x) x %in% conc[["l1mode_nace"]]
  DT0pre <- DT0[!DT0[["post_break"]]]
  goodmodelist <- unique(DT0pre[in_conc(DT0pre[["mode_nace"]]), "mode_nace", with = FALSE])
  goodmodefirm <- unique(DT0[goodmodelist, on = "mode_nace", nomatch = 0][, "idvar", with = FALSE])
  DT0modegood <- DT0[goodmodefirm, on = "idvar", nomatch = 0]

  DT0pre_nomode <- DT0pre[!in_conc(DT0pre[["mode_nace"]])]
  goodlist <- unique(DT0pre_nomode[in_conc(DT0pre_nomode[["nace"]]), "nace", with = FALSE])
  goodfirm <- unique(DT0[goodlist, on = "nace", nomatch = 0][, "idvar", with = FALSE])
  goodfirm <- goodfirm[!goodfirm[["idvar"]] %in% goodmodefirm[["idvar"]]]
  DT0good <- DT0[goodfirm, on = "idvar", nomatch = 0]
  set(goodlist, j = "good", value = 1)
  DT0good <- goodlist[DT0good, on = "nace"]
  is_good <- which(DT0good[["good"]] == 1)
  set(DT0good, i = is_good, j = "mode_nace", value = DT0good[["nace"]][is_good])
  DT0good[, ("mode_nace") := {
    good <- .SD[["good"]] == 1
    .SD[["mode_nace"]][good][which.max(.SD[["year"]][good])]
  }, by = "idvar", .SDcols = c("mode_nace", "good", "year")]
  set(DT0good, j = "good", value = NULL)

  # For firms only before the break, with no available concordances,
  # concordance according to path of other firms with same pre break Nace codes, if possible
  badlist <- unique(DT0pre_nomode[!in_conc(DT0pre_nomode[["nace"]]), "mode_nace", with = FALSE])
  badfirm <- unique(DT0[badlist, on = "mode_nace", nomatch = 0][, "idvar", with = FALSE])
  badfirm <- badfirm[!badfirm[["idvar"]] %in% goodfirm[["idvar"]]]
  DT0bad <- DT0[badfirm, on = "idvar", nomatch = 0]

  DTc_pre <- DTc[!DTc[["post_break"]]]
  badlist_adjusted <- DTc_pre[badlist, on = c(nace = "mode_nace"), nomatch = 0][, c("nace", "mode_nace", "wtvar"), with = FALSE]
  badlist_adjusted[, ("wtvar") := .SD[["wtvar"]] / sum(.SD[["wtvar"]]), by = "nace", .SDcols = "wtvar"]
  setorderv(badlist_adjusted, c("nace", "wtvar"), c(1, -1))
  .add_cum_weights(badlist_adjusted, "nace")
  badlist_adjusted[, ("assign") := runif(1), by = "nace"]
  badlist_adjusted <- badlist_adjusted[badlist_adjusted[["assign"]] <= badlist_adjusted[["cum_wtvar"]] &
                                         badlist_adjusted[["assign"]] > badlist_adjusted[["cum_wtvar_lag"]],
                                       c("nace", "mode_nace"), with = FALSE]
  set(DT0bad, j = "mode_nace", value = NULL)
  DT0bad <- badlist_adjusted[DT0bad, on = "nace", nomatch = NA]

  # every row of a firm takes its mode in the first post-break year
  first_post_break <- function(dt) {
    first <- unique(dt[which(dt[["post_break"]]), c("idvar", "mode_nace"), with = FALSE], by = "idvar")
    set(dt, j = "mode_nace", value = first[["mode_nace"]][match(dt[["idvar"]], first[["idvar"]])])
  }
  first_post_break(DT1)
  first_post_break(DTc)

  # Concordance weights
  conc[, ("wtvar") := .SD[["wtvar"]] / sum(.SD[["wtvar"]]), by = "l1mode_nace", .SDcols = "wtvar"]
  setorderv(conc, c("l1mode_nace", "wtvar"), c(1, -1))
  .add_cum_weights(conc, "l1mode_nace")

  for (dt in list(DT0modegood, DT0good)) {
    dt[, ("assign") := runif(1), by = "idvar"]
    set(dt, j = "l1mode_nace", value = dt[["mode_nace"]])
    tmp <- unique(dt[, c("idvar", "l1mode_nace", "assign"), with = FALSE])[conc, on = "l1mode_nace", allow.cartesian = TRUE]
    tmp <- tmp[tmp[["cum_wtvar_lag"]] < tmp[["assign"]] & tmp[["cum_wtvar"]] >= tmp[["assign"]]]
    dt[tmp[, c("idvar", "mode_nace"), with = FALSE], on = "idvar", ("mode_nace") := mget("i.mode_nace")]
    set(dt, j = "assign", value = NULL)
  }

  # Row bind back to full dataset
  DT <- rbind(DT1, DTc, DT0modegood, DT0good, DT0bad)[, c("idvar", "year", "nace", "mode_nace"), with = FALSE]
  setorderv(DT, c("idvar", "year"))
  setnames(DT, c("mode_nace"), c("MDnace"))

  # Rename idvar back to original key_id for merge
  setnames(MDDT, key_id, "idvar")
  MDDT <- DT[MDDT, on = c("idvar", "year", "nace"), nomatch = NA]
  setnames(MDDT, "idvar", key_id)

  return(MDDT)
}

# The concordance's types (1:1, 1:m, m:1, m:m; each m:m block its own type,
# m:m<n>) and the weights normalized within each 1:m/m:1 origin code or m:m
# block.
.nace_conc_types <- function(conc) {
  conc <- mdi_classify_conc(conc, left = "l1mode_nace", right = "mode_nace", mm_groups = TRUE, mergeconc = TRUE)
  type <- conc[["conctype"]]
  set(conc, j = "conctype", value = ifelse(type == "m:m", paste0(type, conc[["compnum"]]), type))
  conc <- conc[, c("l1mode_nace", "mode_nace", "wtvar", "conctype"), with = FALSE]
  set(conc, j = "wtvar", value = as.numeric(conc[["wtvar"]]))
  is_mm <- substr(conc[["conctype"]], 1, 3) == "m:m"
  conc[!is_mm, ("wtvar") := .SD[["wtvar"]] / sum(.SD[["wtvar"]]), by = "l1mode_nace", .SDcols = "wtvar"]
  conc[is_mm, ("wtvar") := .SD[["wtvar"]] / sum(.SD[["wtvar"]]), by = "conctype", .SDcols = "wtvar"]
  conc
}

# Cumulative weight within each group, and the one before it: a uniform draw
# between the two picks that row.
.add_cum_weights <- function(dt, by) {
  dt[, ("cum_wtvar") := cumsum(.SD[["wtvar"]]), by = by, .SDcols = "wtvar"]
  dt[, ("cum_wtvar_lag") := shift(.SD[["cum_wtvar"]], 1, 0), by = by, .SDcols = "cum_wtvar"]
  invisible(dt)
}

# Fill NAs in x from the previous ("locf") or next ("nocb") non-NA value of
# the same group; rows of a group must be contiguous (DT is keyed by idvar).
.fill_in_group <- function(x, group, type) {
  idx <- seq_along(x)
  idx[is.na(x)] <- NA_integer_
  idx <- nafill(idx, type)
  idx[which(group[idx] != group)] <- NA_integer_
  x[idx]
}

# The previous row's value of x within the same group (NA in a group's first
# row); rows of a group must be contiguous.
.lag_in_group <- function(x, group) {
  out <- shift(x, 1L, type = "lag")
  out[which(shift(group, 1L, type = "lag") != group)] <- out[NA_integer_]
  out
}

# Set the random seed for the rest of the calling function, and put the
# caller's random state back when it returns (CRAN: a package must not change
# the user's random numbers).
.with_local_seed <- function(seed, envir = parent.frame()) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv())
  restore <- bquote({
    if (.(had_seed)) assign(".Random.seed", .(old_seed), envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  })
  do.call(on.exit, list(restore, add = TRUE), envir = envir)
  set.seed(seed)
}
