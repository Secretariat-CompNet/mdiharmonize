test_that("log_warnings writes and appends to the file in path", {
  dir <- paste0(tempfile("log_"), "/")
  dir.create(dir)
  log_warnings("first", dir, append = FALSE)
  log_warnings(c("second", "third"), dir)
  expect_equal(readLines(file.path(dir, "WarningsDescription.txt")), c("first", "second", "third"))
  log_warnings("own file", dir, append = FALSE, file_name = "other.txt")
  expect_equal(readLines(file.path(dir, "other.txt")), "own file")
})

test_that("log_warnings needs a path", {
  expect_error(log_warnings("x"), "'path' is required")
  expect_error(log_warnings("x", NA_character_), "non-empty")
  expect_error(log_warnings("x", c("a/", "b/")), "non-empty")
})
