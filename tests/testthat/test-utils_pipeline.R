test_that(".mode returns the most frequent value, the first to appear on a tie", {
  expect_equal(.mode(c("b", "a", "b")), "b")
  expect_equal(.mode(c("b", "a", "b", "a")), "b")
  expect_equal(.mode(c(10, 9, 10, 9)), 10)
  expect_equal(.mode(c(NA, "a", NA)), "a")
})

test_that(".mode returns NA of the input's type when there is no value", {
  expect_identical(.mode(c(NA_character_, NA)), NA_character_)
  expect_identical(.mode(numeric(0)), NA_real_)
  expect_identical(.mode(c(NA_integer_)), NA_integer_)
})
