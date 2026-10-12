make_flat_dt <- function() {
  dt <- data.table::data.table(
    nace        = c("A", "B", "C"),
    sum_emp     = c(100, 200, 40),
    mean_emp    = c(5, 8, 2),
    domPerc_emp = c(0.50, 0.92, 0.20),
    NumObs      = c(10L, 10L, 3L)
  )
  data.table::setkeyv(dt, "nace")
  dt
}

test_that("mdi_disclose suppresses cells below minNumObs and applies dominance to sum-columns only (domAllCols = 0)", {
  out <- mdi_disclose(make_flat_dt(), hier = FALSE, minNumObs = 5, domSh = 0.85, domAllCols = 0)

  # A: passes every rule, untouched
  expect_equal(out[nace == "A"]$sum_emp, 100)
  expect_equal(out[nace == "A"]$mean_emp, 5)
  expect_true(is.na(out[nace == "A"]$discflag1))

  # B: dominant (domPerc_emp >= domSh) -- only sum_emp masked, mean_emp untouched
  expect_equal(out[nace == "B"]$sum_emp, -999)
  expect_equal(out[nace == "B"]$mean_emp, 8)
  expect_equal(out[nace == "B"]$discflag1_domPerc_emp, -999)

  # C: below minNumObs -- every column suppressed
  expect_equal(out[nace == "C"]$sum_emp, -999)
  expect_equal(out[nace == "C"]$mean_emp, -999)
  expect_equal(out[nace == "C"]$discflag1, -999)
})

test_that("mdi_disclose's domAllCols = 1 extends dominance masking to every statistic owned by the dominant variable", {
  out <- mdi_disclose(make_flat_dt(), hier = FALSE, minNumObs = 5, domSh = 0.85, domAllCols = 1)

  # B: dominance now also masks mean_emp (domAllCols = 1), not just sum_emp
  expect_equal(out[nace == "B"]$sum_emp, -999)
  expect_equal(out[nace == "B"]$mean_emp, -999)
})

test_that("mdi_disclose pairs domPerc_<var> with its own sum-column by name, whatever the column order (domAllCols = 0)", {
  dt <- data.table::data.table(
    nace = c("A", "B"),
    sum_nfirms = c(10, 10), sum_rev = c(500, 900), sum_emp = c(100, 200),
    domPerc_emp = c(0.10, 0.92), domPerc_rev = c(0.95, 0.10),
    NumObs = 10L
  )
  data.table::setkeyv(dt, "nace")
  out <- mdi_disclose(dt, hier = FALSE, minNumObs = 5, domSh = 0.85, domAllCols = 0)
  # B: emp is dominant -> sum_emp masked; by position it was sum_nfirms
  expect_equal(out[nace == "B"]$sum_emp, -999)
  expect_equal(out[nace == "B"]$sum_nfirms, 10)
  expect_equal(out[nace == "B"]$sum_rev, 900)
  # A: rev is dominant -> sum_rev masked; by position it was sum_rev's neighbour
  expect_equal(out[nace == "A"]$sum_rev, -999)
  expect_equal(out[nace == "A"]$sum_emp, 100)
  expect_equal(out[nace == "A"]$sum_nfirms, 10)
})

test_that("mdi_disclose's domPerc_<var> without a sum-column of <var> masks no other column (domAllCols = 0)", {
  dt <- data.table::data.table(nace = "A", sum_nfirms = 10, mean_emp = 5,
                               domPerc_emp = 0.95, NumObs = 10L)
  data.table::setkeyv(dt, "nace")
  out <- mdi_disclose(dt, hier = FALSE, minNumObs = 5, domSh = 0.85, domAllCols = 0)
  expect_equal(out$sum_nfirms, 10)
  expect_equal(out$mean_emp, 5)
})

test_that("mdi_disclose's dom_direction = 'low' inverts the dominance rule", {
  out <- mdi_disclose(make_flat_dt(), hier = FALSE, minNumObs = 5, domSh = 0.85,
                   domAllCols = 0, dom_direction = "low")

  # A: domPerc_emp (0.50) < domSh (0.85) -- dominant under the inverted rule
  expect_equal(out[nace == "A"]$sum_emp, -999)
  # B: domPerc_emp (0.92) >= domSh -- NOT dominant under the inverted rule
  expect_equal(out[nace == "B"]$sum_emp, 200)
})

test_that("mdi_disclose errors on an unkeyed table", {
  dt <- make_flat_dt()
  data.table::setkeyv(dt, NULL)
  expect_error(mdi_disclose(dt, hier = FALSE, minNumObs = 5), "no data.table key")
})

test_that("mdi_disclose errors when NumObs is missing", {
  dt <- make_flat_dt()
  dt[, NumObs := NULL]
  expect_error(mdi_disclose(dt, hier = FALSE, minNumObs = 5), "NumObs")
})

test_that("mdi_disclose errors on a node column with hier = FALSE and sum-columns present", {
  dt <- make_flat_dt()
  dt[, node := "h_0"]
  expect_error(mdi_disclose(dt, hier = FALSE, minNumObs = 5), "hier = TRUE")
})

test_that("mdi_disclose errors on hier = TRUE with no node column", {
  dt <- make_flat_dt()
  expect_error(mdi_disclose(dt, hier = TRUE, minNumObs = 5), "no 'node' column")
})

test_that("mdi_disclose's secondary disclosure suppresses the lone remaining child when its sibling is already suppressed", {
  hhfile <- data.table::data.table(h_0 = c("A1", "A2"), h_1 = c("A", "A"))
  dt <- data.table::data.table(
    node    = c("h_0", "h_0", "h_1"),
    nace    = c("A1", "A2", "A"),
    sum_emp = c(100, 50, 150),
    NumObs  = c(10L, 2L, 12L)
  )
  data.table::setkeyv(dt, "nace")

  out <- mdi_disclose(dt, hhfile = hhfile, hier = TRUE, minNumObs = 5)

  # A2 fails minNumObs directly (primary disclosure)
  expect_equal(out[nace == "A2"]$sum_emp, -999)
  expect_equal(out[nace == "A2"]$discflag1, -999)

  # A1 is the only other child in its parent group ("A") -- gets caught by
  # secondary disclosure even though it individually passed minNumObs
  expect_equal(out[nace == "A1"]$sum_emp, -999)
  expect_equal(out[nace == "A1"]$discflag2, 1)

  # the parent row must keep its OWN nace/node values -- regression test for a
  # real bug where rbindlist() without use.names = TRUE silently swapped the
  # top-level row's columns with the child rows' (different column order after
  # get_hier()'s merge())
  parent_row <- out[node == "h_1"]
  expect_equal(parent_row$nace, "A")
  expect_equal(parent_row$sum_emp, 150)
})

test_that("mdi_disclose leaves the caller's table unchanged", {
  dt <- make_flat_dt()
  before <- data.table::copy(dt)
  out <- mdi_disclose(dt, hier = FALSE, minNumObs = 5, domSh = 0.85, domAllCols = 0)
  expect_identical(dt, before)
  expect_true(any(out$sum_emp == -999))
})
