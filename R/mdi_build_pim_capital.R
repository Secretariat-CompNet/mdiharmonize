#' Add a Perpetual Inventory Method capital stock to SBS
#'
#' @description
#' Gives each SBS firm-year its NACE 2-digit industry from BR (by default the
#' time-consistent `MDnace`, see `nace_var`), starts each
#' firm's capital stock from its industry's capital per employee (EU KLEMS),
#' accumulates it with [mdi_pim_capital()] from tangible investment
#' (`ni_tan`) and the industry depreciation rate, and returns SBS with a
#' `K_pim` column added.
#'
#' @param sbs A `data.table`, the harmonized SBS MD dataset: `id_sbs`,
#'   `year`, `ni_tan` and `emp_var`.
#' @param br A `data.table`, the harmonized BR MD dataset: `id_br`, `year`
#'   and the industry column `nace_var`.
#' @param capital_stocks A `data.table` in the long format of
#'   `rocket/auxdata/capital_stocks_<country>.csv`: `year`, `nace` (EU KLEMS
#'   codes, e.g. `"A01"`), `cc`, `variable` and `value`. Rows with `variable`
#'   `"K_GFCF_per_emp"` (capital per employee) and `"d_GFCF"` (depreciation
#'   rate) are used.
#' @param nace_hier A `data.table`, `MD_nace_hier`: `h_0` (4-digit), `h_1`
#'   (3-digit) and `h_2` (2-digit) NACE codes.
#' @param id_sbs,id_br Character, the firm identifier column of SBS and of BR
#'   (e.g. `"firmid"`, `"plantid"`). When they differ, BR is brought to SBS's
#'   level with [mdi_key_id_switch()] first.
#' @param conctab A `data.table`, the concordance between `id_br` and
#'   `id_sbs` (needed only when they differ).
#' @param br_varnames A `data.table`, `MD_varnames` filtered to BR (needed
#'   only when `id_br` and `id_sbs` differ).
#' @param emp_var Character, SBS's employment column used to scale the
#'   initial capital. Default `"persons"`.
#' @param nace_var Character, BR's industry column. Default `"MDnace"`, the
#'   time-consistent code from [mdi_concord_nace()]: a firm whose raw `nace`
#'   changes for classification reasons (e.g. the 2008 NACE revision) keeps
#'   one industry, so its depreciation rate doesn't jump. `"nace"` uses the
#'   code as recorded each year.
#'
#' @return `sbs` with a `K_pim` column added, every row kept: a firm-year
#'   with no BR match (so no industry) gets `K_pim = NA`. When BR's NACE
#'   codes are all 3-digit, industries are matched at `h_1` instead of `h_0`.
#'   A `K_pim` already in `sbs` (from an earlier run) is replaced. `sbs`
#'   itself is not modified.
#'
#' @examples
#' yrs <- 2010:2012
#' sbs <- data.table::data.table(firmid = rep(c("f1", "f2"), each = 3), year = rep(yrs, 2),
#'                               persons = 10L, ni_tan = 1:6)
#' br <- data.table::data.table(firmid = rep(c("f1", "f2"), each = 3), year = rep(yrs, 2),
#'                              MDnace = rep(c("0111", "1011"), each = 3))
#' capital_stocks <- data.table::data.table(
#'   year = rep(yrs, 4), nace = rep(c("A01", "A01", "C10", "C10"), each = 3), cc = "XX",
#'   variable = rep(c("K_GFCF_per_emp", "d_GFCF"), each = 3, times = 2),
#'   value = rep(c(100, 0.1, 200, 0.2), each = 3)
#' )
#' nace_hier <- data.table::data.table(h_0 = c("0111", "1011"), h_1 = c("011", "101"),
#'                                     h_2 = c("01", "10"))
#' mdi_build_pim_capital(sbs, br, capital_stocks, nace_hier, id_sbs = "firmid")
#' @export
mdi_build_pim_capital <- function(sbs, br, capital_stocks, nace_hier, id_sbs,
                                  id_br = id_sbs, conctab = NULL,
                                  br_varnames = NULL, emp_var = "persons",
                                  nace_var = "MDnace") {
  check_string(id_sbs, "id_sbs")
  check_string(id_br, "id_br")
  check_string(emp_var, "emp_var")
  check_string(nace_var, "nace_var")
  check_dt(sbs, c(id_sbs, "year", "ni_tan", emp_var), "sbs")
  check_dt(br, c(id_br, "year", nace_var), "br")
  check_dt(capital_stocks, c("year", "nace", "cc", "variable", "value"), "capital_stocks")
  check_dt(nace_hier, c("h_0", "h_1", "h_2"), "nace_hier")
  if (id_br != id_sbs && (is.null(conctab) || is.null(br_varnames))) {
    stop("mdi_build_pim_capital(): BR ('", id_br, "') and SBS ('", id_sbs, "') are at ",
         "different firm levels, so 'conctab' and 'br_varnames' are needed to switch BR ",
         "to SBS's level.")
  }
  # an SBS from an earlier run already has K_pim: replace it
  sbs_cols <- setdiff(names(sbs), "K_pim")
  if ("K_pim" %in% names(sbs)) sbs <- sbs[, sbs_cols, with = FALSE]

  # ---- BR at SBS's firm level ----
  if (id_sbs != id_br) {
    id_hierarchy <- .firm_id_levels
    switch_method <- if (match(id_sbs, id_hierarchy) > match(id_br, id_hierarchy)) "agg" else "disagg"
    # persons_br, where present, weights the industry's aggregation (weighted_mode)
    br <- mdi_key_id_switch(
      DT = br, conctab = conctab, md_varnames = br_varnames,
      method = switch_method, obj_id_type = paste0("to_", id_sbs),
      cols = intersect(c(id_br, "year", nace_var, "persons_br"), names(br)), key_id = id_br
    )
  }

  # ---- NACE 2-digit industry per firm-year ----
  br_nace <- br[, c(nace_var, id_sbs, "year"), with = FALSE]
  code <- as.character(br_nace[[nace_var]])
  data.table::set(br_nace, j = ".nace4", value = substr(code, 1, 4))
  # 3-digit (group-level) NACE codes don't match h_0 (4-digit): join at h_1
  # instead, unique'd so the right-hand side doesn't multiply rows
  nace_level <- if (all(nchar(code[!is.na(code)]) <= 3)) "h_1" else "h_0"
  br_nace <- merge(br_nace, unique(nace_hier[, c(nace_level, "h_2"), with = FALSE]),
                   by.x = ".nace4", by.y = nace_level, all.x = TRUE)
  br_nace <- unique(br_nace[, c(id_sbs, "year", "h_2"), with = FALSE],
                    by = c(id_sbs, "year"))

  # every SBS row is kept: one without a BR match just gets no industry, so
  # no K_pim
  sbs <- merge(sbs, br_nace, by = c(id_sbs, "year"), all.x = TRUE)

  # ---- industry capital per employee and depreciation ----
  kl <- capital_stocks[capital_stocks[["variable"]] %in% c("K_GFCF_per_emp", "d_GFCF")]
  kl <- data.table::dcast(kl, year + nace + cc ~ variable, value.var = "value")
  data.table::set(kl, j = "cc", value = NULL)
  # "A01" -> "01", matching MD_nace_hier's zero-padded h_2
  data.table::set(kl, j = "h_2", value = substr(kl[["nace"]], 2, 3))
  data.table::setkeyv(kl, c("h_2", "year"))
  sbs <- kl[sbs, on = c("h_2", "year")]

  # ---- initial capital, investment, PIM ----
  emp <- sbs[[emp_var]]
  data.table::set(sbs, j = "firm_K0", value = NA_real_)
  has_emp <- which(emp != 0)
  data.table::set(sbs, i = has_emp, j = "firm_K0",
                  value = sbs[["K_GFCF_per_emp"]][has_emp] * emp[has_emp])
  data.table::set(sbs, j = "tan_invest", value = sbs[["ni_tan"]] * 1000)

  sbs <- mdi_pim_capital(DT = sbs, id = id_sbs, K0 = "firm_K0", delta = "d_GFCF",
                         output_name = "K_pim", I = "tan_invest", time = "year")

  sbs[, c(sbs_cols, "K_pim"), with = FALSE]
}
