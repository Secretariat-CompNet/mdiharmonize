make_pim_inputs <- function(nace = c("0111", "1011")) {
  yrs <- 2010:2012
  list(
    sbs = data.table::data.table(firmid = rep(c("f1", "f2"), each = 3), year = rep(yrs, 2),
                                 persons = 10L, ni_tan = c(1, 2, 3, 4, 5, 6)),
    br = data.table::data.table(firmid = rep(c("f1", "f2"), each = 3), year = rep(yrs, 2),
                                nace = rep(nace, each = 3), MDnace = rep(nace, each = 3)),
    capital_stocks = data.table::rbindlist(list(
      data.table::data.table(year = yrs, nace = "A01", cc = "XX", variable = "K_GFCF_per_emp", value = 100),
      data.table::data.table(year = yrs, nace = "A01", cc = "XX", variable = "d_GFCF", value = 0.1),
      data.table::data.table(year = yrs, nace = "C10", cc = "XX", variable = "K_GFCF_per_emp", value = 200),
      data.table::data.table(year = yrs, nace = "C10", cc = "XX", variable = "d_GFCF", value = 0.2)
    )),
    nace_hier = data.table::data.table(h_0 = c("0111", "1011"), h_1 = c("011", "101"),
                                       h_2 = c("01", "10"))
  )
}

test_that("mdi_build_pim_capital adds K_pim from the firm's NACE 2-digit industry", {
  x <- make_pim_inputs()
  out <- mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier, id_sbs = "firmid")

  expect_equal(names(out), c(names(x$sbs), "K_pim"))
  # first year: initial capital = industry capital per employee * persons
  expect_equal(out[firmid == "f1" & year == 2010]$K_pim, 100 * 10)
  expect_equal(out[firmid == "f2" & year == 2010]$K_pim, 200 * 10)
  # later years accumulate with the industry's depreciation and investment
  expect_true(all(!is.na(out$K_pim)))
})

test_that("mdi_build_pim_capital replaces a K_pim from an earlier run", {
  x <- make_pim_inputs()
  first <- mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier, id_sbs = "firmid")
  first[, K_pim := -1]
  again <- mdi_build_pim_capital(first, x$br, x$capital_stocks, x$nace_hier, id_sbs = "firmid")
  expect_equal(names(again), c(names(x$sbs), "K_pim"))
  expect_equal(again$K_pim, mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier,
                                                  id_sbs = "firmid")$K_pim)
})

test_that("mdi_build_pim_capital matches 3-digit NACE codes at h_1", {
  x <- make_pim_inputs(nace = c("011", "101"))
  out <- mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier, id_sbs = "firmid")
  expect_equal(out[firmid == "f2" & year == 2010]$K_pim, 200 * 10)
})

test_that("mdi_build_pim_capital does not modify its inputs", {
  x <- make_pim_inputs()
  before <- lapply(x, data.table::copy)
  mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier, id_sbs = "firmid")
  for (n in names(x)) expect_identical(x[[n]], before[[n]])
})

test_that("mdi_build_pim_capital keeps SBS rows that have no BR match", {
  x <- make_pim_inputs()
  x$br <- x$br[firmid == "f1"]  # f2 is in SBS but not BR
  out <- mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier, id_sbs = "firmid")
  expect_equal(nrow(out), nrow(x$sbs))
  expect_true(all(is.na(out[firmid == "f2"]$K_pim)))
  expect_true(all(!is.na(out[firmid == "f1"]$K_pim)))
})

test_that("mdi_build_pim_capital brings a plant-level BR to SBS's firm level", {
  x <- make_pim_inputs()
  # two plants of f1 in NACE 01 (the bigger one decides), one plant of f2 in 10
  x$br <- data.table::data.table(
    plantid = rep(c("p1", "p2", "p3"), each = 3), year = rep(2010:2012, 3),
    nace = rep(c("0111", "0111", "1011"), each = 3), persons_br = rep(c(5L, 3L, 4L), each = 3)
  )
  x$br[, MDnace := nace]
  conctab <- data.table::data.table(plantid = rep(c("p1", "p2", "p3"), each = 3),
                                    firmid = rep(c("f1", "f1", "f2"), each = 3),
                                    year = rep(2010:2012, 3))
  br_varnames <- data.table::data.table(
    MD_dataset = "BR", MD_varname = c("plantid", "firmid", "year", "nace", "MDnace", "persons_br"),
    is_key = c(1L, 1L, 1L, 0L, 0L, 0L),
    aggregation_method = c("", "", "", "weighted_mode:persons_br|mode", "weighted_mode:persons_br|mode", "sum"),
    disaggregation_method = c("", "", "", "replicate", "replicate", "replicate")
  )
  out <- mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier,
                               id_sbs = "firmid", id_br = "plantid",
                               conctab = conctab, br_varnames = br_varnames)
  expect_equal(nrow(out), nrow(x$sbs))
  expect_equal(out[firmid == "f1" & year == 2010]$K_pim, 100 * 10)
  expect_equal(out[firmid == "f2" & year == 2010]$K_pim, 200 * 10)
})

test_that("mdi_build_pim_capital needs a concordance when BR is at another firm level", {
  x <- make_pim_inputs()
  data.table::setnames(x$br, "firmid", "plantid")
  expect_error(
    mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier,
                          id_sbs = "firmid", id_br = "plantid"),
    "conctab"
  )
})

test_that("mdi_build_pim_capital validates its inputs", {
  x <- make_pim_inputs()
  expect_error(mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier,
                                     id_sbs = "firmid", emp_var = "emp"), "emp")
  expect_error(mdi_build_pim_capital(x$sbs[, !"ni_tan"], x$br, x$capital_stocks, x$nace_hier,
                                     id_sbs = "firmid"), "ni_tan")
})

test_that("mdi_build_pim_capital takes the industry from MDnace, or from nace_var", {
  x <- make_pim_inputs()
  # f1's raw code switches industry in 2011 (a reclassification), its
  # time-consistent MDnace doesn't
  x$br[firmid == "f1" & year == 2011, nace := "1011"]
  by_md <- mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier, id_sbs = "firmid")
  by_raw <- mdi_build_pim_capital(x$sbs, x$br, x$capital_stocks, x$nace_hier, id_sbs = "firmid", nace_var = "nace")
  plain <- mdi_build_pim_capital(make_pim_inputs()$sbs, make_pim_inputs()$br, x$capital_stocks, x$nace_hier,
                                 id_sbs = "firmid")
  # MDnace: f1 stays in industry 01 throughout, as if its code had never changed
  expect_equal(by_md$K_pim, plain$K_pim)
  # nace: f1's 2011 depreciation comes from industry 10, so its stock differs from then on
  expect_false(isTRUE(all.equal(by_raw[firmid == "f1"]$K_pim, plain[firmid == "f1"]$K_pim)))
  expect_equal(by_raw[firmid == "f2"]$K_pim, plain[firmid == "f2"]$K_pim)
  expect_error(mdi_build_pim_capital(x$sbs, x$br[, !"MDnace"], x$capital_stocks, x$nace_hier, id_sbs = "firmid"),
               "MDnace")
})
