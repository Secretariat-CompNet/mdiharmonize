make_code_chain <- function() {
  # A (2010) became B in 2011, which became C in 2012; X never changed
  list(
    conc = data.table::data.table(left = c("A", "X", "B", "X"), right = c("B", "X", "C", "X"),
                                  year = c(2011, 2011, 2012, 2012)),
    dt = data.table::data.table(year = c(2010, 2011, 2012, 2012), cn08 = c("A", "B", "C", "X"),
                                v = 1:4)
  )
}

test_that("mdi_harmonize_codes_over_time maps every year's code to the last year's", {
  x <- make_code_chain()
  out <- mdi_harmonize_codes_over_time(x$dt, x$conc, "cn08", data_years = c(2010, 2012))
  data.table::setorder(out$data, year, NSI_cn08)
  expect_equal(out$data$cn08, c("C", "C", "C", "X"))
  expect_equal(out$data$NSI_cn08, c("A", "B", "C", "X"))
  expect_equal(out$data$v, 1:4)
})

test_that("mdi_harmonize_codes_over_time returns the mapping table it used", {
  x <- make_code_chain()
  out <- mdi_harmonize_codes_over_time(x$dt, x$conc, "cn08", data_years = c(2010, 2012))
  expect_equal(names(out$codes), c("year", "cn08", "cn08_harmonized"))
  expect_equal(out$codes[year == 2010 & cn08 == "A"]$cn08_harmonized, "C")
})

test_that("mdi_harmonize_codes_over_time does not modify dt", {
  x <- make_code_chain()
  before <- data.table::copy(x$dt)
  mdi_harmonize_codes_over_time(x$dt, x$conc, "cn08", data_years = c(2010, 2012))
  expect_identical(x$dt, before)
})

test_that("mdi_harmonize_codes_over_time validates its inputs", {
  x <- make_code_chain()
  expect_error(mdi_harmonize_codes_over_time(x$dt, x$conc, "pcc8", c(2010, 2012)), "pcc8")
  expect_error(mdi_harmonize_codes_over_time(x$dt, x$conc, "cn08", 2010), "data_years")
})

test_that("mdi_harmonize_codes_over_time matches codes written with dots or spaces", {
  conc <- data.table::data.table(left = c("10111100", "10111200"),
                                 right = c("10111100", "10111300"), year = 2011)
  dt <- data.table::data.table(year = c(2010, 2011), cn08 = c("10.11.11.00", "10 11 13 00"))
  out <- mdi_harmonize_codes_over_time(dt, conc, "cn08", data_years = c(2010, 2011))
  data.table::setorder(out$data, year)
  expect_equal(out$data$cn08, c("10111100", "10111300"))
  expect_equal(out$data$NSI_cn08, c("10.11.11.00", "10 11 13 00"))
})

test_that("mdi_harmonize_codes_over_time warns and returns NULL when the years don't overlap", {
  dt <- data.table::data.table(year = c(2016, 2017), cn08 = c("1", "2"))
  conc <- data.table::data.table(left = c("1", "2"), right = c("1", "2"), year = c(2014, 2015))
  expect_warning(out <- mdi_harmonize_codes_over_time(dt, conc, "cn08", c(2016, 2020)),
                 "doesn't overlap")
  expect_null(out)
})
