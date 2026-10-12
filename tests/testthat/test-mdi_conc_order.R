make_conctype <- function() {
  # every conctype, rows deliberately interleaved so 1:1 rows don't come first
  conc <- data.table::data.table(
    NSI_dataset = c("p", "b", "a", "q", "b", "c", "p", "d", "q", "e"),
    MD_dataset  = c("P1", "B1", "A", "P1", "B2", "C", "P2", "C", "P2", "E")
  )
  mdi_classify_conc(conc, left = "NSI_dataset", right = "MD_dataset",
            mm_groups = TRUE, mergeconc = FALSE)
}

# expected values taken from the original efficient_conc_order.R script
test_that("mdi_conc_order reproduces the original script's order and flags", {
  out <- mdi_conc_order(make_conctype())

  expect_equal(out$NSI_dataset, c("a", "e", "b", "b", "c", "d", "p", "q", "p", "q"))
  expect_equal(out$MD_dataset,  c("A", "E", "B1", "B2", "C", "C", "P1", "P1", "P2", "P2"))
  expect_equal(out$ordernum,    c(1, 2, 3, 3, 4, 4, 5, 5, 5, 5))
  expect_equal(out$openNSI,     c(1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 0L, 0L))
  expect_equal(out$closeNSI,    c(1L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 1L, 1L))
  expect_equal(out$closeMD,     c(1L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 1L))
})

test_that("mdi_conc_order numbers 1:1 pairs 1, 2, ... regardless of their row position", {
  out <- mdi_conc_order(make_conctype())
  expect_equal(out[conctype == "1:1"]$ordernum, c(1, 2))
})

test_that("mdi_conc_order does not modify its input", {
  conc <- make_conctype()
  before <- data.table::copy(conc)
  mdi_conc_order(conc)
  expect_identical(names(conc), names(before))
  expect_identical(conc$NSI_dataset, before$NSI_dataset)
})

test_that("mdi_conc_order picks the m:m build order that keeps the fewest idle NSI datasets", {
  # X2 needs x and y. Building X2 in the middle (X1 -> X2 -> X3, or the
  # reverse) never holds an unneeded dataset; building it first or last does.
  conc <- data.table::data.table(
    NSI_dataset = c("x", "x", "y", "y"),
    MD_dataset  = c("X1", "X2", "X2", "X3")
  )
  ct <- mdi_classify_conc(conc, left = "NSI_dataset", right = "MD_dataset",
                  mm_groups = TRUE, mergeconc = FALSE)
  out <- mdi_conc_order(ct)
  expect_equal(unique(out$MD_dataset)[2], "X2")
})

test_that("mdi_conc_order works without any m:m pairs", {
  conc <- data.table::data.table(NSI_dataset = c("a", "b"), MD_dataset = c("A", "B"))
  ct <- mdi_classify_conc(conc, left = "NSI_dataset", right = "MD_dataset",
                  mm_groups = TRUE, mergeconc = FALSE)
  out <- mdi_conc_order(ct)
  expect_equal(out$ordernum, c(1, 2))
  expect_false("sort_order" %in% names(out))
})

test_that("mdi_conc_order errors on missing columns", {
  expect_error(mdi_conc_order(data.table::data.table(NSI_dataset = "a")), "columns not found")
})
