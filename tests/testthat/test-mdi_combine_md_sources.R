make_sources <- function() {
  list(
    data.table::data.table(firmid = c("a", "b"), year = 2020, nace = c("10", NA), emp = 1:2),
    data.table::data.table(firmid = c("b", "c"), year = 2020, nace = c("20", "30"), turn = c(5, 6))
  )
}

test_that("mdi_combine_md_sources merges on id and year, keeping every row", {
  res <- mdi_combine_md_sources(make_sources(), c("firmid", "year"), "merge")
  expect_equal(res$firmid, c("a", "b", "c"))
  expect_true(all(c("nace.x", "nace.y") %in% names(res)))
})

test_that("mdi_combine_md_sources coalesces duplicated columns of a merge", {
  res <- mdi_combine_md_sources(make_sources(), c("firmid", "year"), "merge", coalesce_duplicates = TRUE)
  expect_false(any(grepl("\\.(x|y)$", names(res))))
  expect_equal(res$nace, c("10", "20", "30"))
})

test_that("mdi_combine_md_sources warns when the two sources disagree", {
  src <- make_sources()
  src[[1]]$nace <- c("10", "99")
  expect_warning(res <- mdi_combine_md_sources(src, c("firmid", "year"), "merge", coalesce_duplicates = TRUE),
                 "'nace'.*1 row")
  expect_equal(res$nace, c("10", "99", "30"))
})

test_that("mdi_combine_md_sources coalesces factors with different levels as character", {
  src <- make_sources()
  src[[1]]$nace <- factor(src[[1]]$nace)
  res <- mdi_combine_md_sources(src, c("firmid", "year"), "merge", coalesce_duplicates = TRUE)
  expect_equal(res$nace, c("10", "20", "30"))
})

test_that("mdi_combine_md_sources stacks sources", {
  res <- mdi_combine_md_sources(make_sources(), c("firmid", "year"), "stack")
  expect_equal(nrow(res), 4L)
  expect_true(all(c("emp", "turn") %in% names(res)))
})

test_that("mdi_combine_md_sources sets the weight variable to 1 on a merge", {
  src <- make_sources()
  src[[1]]$weights_sbs_nsi <- c(3, 4)
  res <- mdi_combine_md_sources(src, c("firmid", "year"), "merge", weight_var = "weights_sbs_nsi")
  expect_equal(res$weights_sbs_nsi, c(1, 1, 1))
})

test_that("mdi_combine_md_sources keeps each row's own weight on a stack", {
  src <- make_sources()
  src[[1]]$weights_sbs_nsi <- c(3, 4)
  src[[2]]$weights_sbs_nsi <- c(7, 8)
  res <- mdi_combine_md_sources(src, c("firmid", "year"), "stack", weight_var = "weights_sbs_nsi")
  expect_equal(res$weights_sbs_nsi, c(3, 4, 7, 8))
})

test_that("mdi_combine_md_sources validates its arguments", {
  expect_error(mdi_combine_md_sources(list(), "firmid"), "dt_list")
  expect_error(mdi_combine_md_sources(make_sources(), "firmid", "append"), "arg")
})
