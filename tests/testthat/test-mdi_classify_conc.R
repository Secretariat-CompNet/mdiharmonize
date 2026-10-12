test_that("mdi_classify_conc returns a data.table with conctype column", {
  DTconc <- data.table::data.table(
    leftx  = as.character(c(1, 2, 2, 3, 4, 5)),
    rightx = c("A", "B", "C", "D", "D", "F")
  )
  result <- mdi_classify_conc(DTconc, left = "leftx", right = "rightx",
                      mm_groups = FALSE, mergeconc = FALSE)
  expect_s3_class(result, "data.table")
  expect_true("conctype" %in% names(result))
  expect_true(all(result$conctype %in% c("1:1", "1:m", "m:1", "m:m")))
})

test_that("mdi_classify_conc 1:1 is correctly classified", {
  DTconc <- data.table::data.table(left = "A", right = "X")
  result <- mdi_classify_conc(DTconc, mm_groups = FALSE)
  expect_equal(result$conctype, "1:1")
})

test_that("mdi_classify_conc mergeconc=TRUE returns merged table", {
  DTconc <- data.table::data.table(
    leftx  = as.character(c(1, 2, 3)),
    rightx = c("A", "B", "C")
  )
  result <- mdi_classify_conc(DTconc, left = "leftx", right = "rightx",
                      mm_groups = FALSE, mergeconc = TRUE)
  expect_true("leftx" %in% names(result))
  expect_true("rightx" %in% names(result))
})

test_that("mdi_classify_conc keeps a code that appears on both sides as two nodes", {
  # two separate m:m blocks; "0112" is a new code in the first and an old
  # code in the second
  DTconc <- data.table::data.table(
    left  = c("0111", "0111", "0113", "0112", "0112", "0114"),
    right = c("0111", "0112", "0112", "0115", "0116", "0116")
  )
  result <- mdi_classify_conc(DTconc)
  expect_true(all(result$conctype == "m:m"))
  expect_equal(data.table::uniqueN(result$compnum), 2L)
  expect_equal(data.table::uniqueN(result[left == "0112"]$compnum), 1L)
  expect_false(result[left == "0112"]$compnum[1] == result[left == "0111"]$compnum[1])
})
