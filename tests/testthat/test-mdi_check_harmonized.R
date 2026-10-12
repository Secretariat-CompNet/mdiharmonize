make_phc_catalogue <- function() {
  data.table::data.table(
    MD_dataset = "SBS",
    MD_varname = c("firmid", "year", "emp", "turnover", "birthdt", "name"),
    is_key = c(1L, 1L, 0L, 0L, 0L, 0L),
    class = c("character", "numeric", "numeric", "numeric", "date", "character"),
    domain = c(NA, NA, NA, NA, "%d%m%Y", NA),
    FI = "2019 - 2021"
  )
}

make_phc_data <- function() {
  data.table::data.table(
    firmid = c("a", "b", "a", "b", "a", "a"),
    year = c(2019, 2019, 2020, 2020, 2021, 2021),
    emp = c(10, 20, 10, 20, 30, 40),
    turnover = c(1, 1, 1, 1, 1, 1),
    birthdt = c("01012000", "15062001", NA, "01012000", "31122010", "01012000"),
    name = 1:6
  )
}

test_that("mdi_check_harmonized counts duplicated keys", {
  res <- mdi_check_harmonized(make_phc_data(), "SBS", make_phc_catalogue(), "FI")
  expect_equal(res$duplicates$id_var, "firmid, year")
  expect_true(res$duplicates$has_duplicates)
  expect_equal(res$duplicates$num_duplicated_rows, 1L)
  expect_equal(res$duplicates$num_unique_duplicated_keys, 1L)
})

test_that("mdi_check_harmonized reports NA duplicates when no key variable is present", {
  dt <- make_phc_data()[, c("emp", "year"), with = FALSE]
  cat <- make_phc_catalogue()
  cat[["is_key"]] <- c(1L, 0L, 0L, 0L, 0L, 0L)
  res <- mdi_check_harmonized(dt, "SBS", cat, "FI")
  expect_true(is.na(res$duplicates$has_duplicates))
})

test_that("mdi_check_harmonized counts rows without a year", {
  res <- mdi_check_harmonized(make_phc_data(), "SBS", make_phc_catalogue(), "FI")
  expect_equal(res$years$num_rows_no_year, 0L)

  dt <- make_phc_data()
  dt[["year"]] <- NA_integer_
  res <- mdi_check_harmonized(dt, "SBS", make_phc_catalogue(), "FI")
  expect_equal(res$years$num_rows_no_year, 6L)

  res <- mdi_check_harmonized(make_phc_data()[, !"year"], "SBS", make_phc_catalogue(), "FI")
  expect_false(res$years$has_year)
  expect_equal(res$years$num_rows_no_year, 6L)
})

test_that("mdi_check_harmonized compares declared and actual classes", {
  res <- mdi_check_harmonized(make_phc_data(), "SBS", make_phc_catalogue(), "FI")
  expect_equal(res$classes$variable, c("firmid", "year", "emp", "turnover", "name"))
  expect_equal(res$classes$class_match, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(res$date_formats$variable, "birthdt")
  expect_equal(res$date_formats$actual_format, "%d%m%Y")
  expect_true(res$date_formats$format_valid)
  expect_false(res$date_formats$class_match)
})

test_that("mdi_check_harmonized treats a date without a declared format as a class check", {
  cat <- make_phc_catalogue()
  cat[["domain"]][5] <- ""
  res <- mdi_check_harmonized(make_phc_data(), "SBS", cat, "FI")
  expect_null(res$date_formats)
  expect_true("birthdt" %in% res$classes$variable)
})

test_that("mdi_check_harmonized reports the dates the data covers", {
  res <- mdi_check_harmonized(make_phc_data(), "SBS", make_phc_catalogue(), "FI")
  expect_equal(res$date_ranges$actual_min_date, as.Date("2000-01-01"))
  expect_equal(res$date_ranges$actual_max_date, as.Date("2010-12-31"))
  expect_equal(res$date_ranges$expected_range, "2019 - 2021")
})

test_that("mdi_check_harmonized flags yearly statistics that jump past the threshold", {
  res <- mdi_check_harmonized(make_phc_data(), "SBS", make_phc_catalogue(), "FI")
  expect_equal(unique(res$breaks$variable), "emp")
  expect_true(all(res$breaks$year == 2021))
  # sd is sqrt(50) in 2020 and 2021 alike
  expect_setequal(res$breaks$stat, c("p25", "p50", "p75", "mean"))
  expect_equal(names(res$series), "emp")
  expect_equal(res$series$emp$mean, c(15, 15, 35))
})

test_that("mdi_check_harmonized checks only the requested break variables", {
  res <- mdi_check_harmonized(make_phc_data(), "SBS", make_phc_catalogue(), "FI",
                              break_vars = "turnover")
  expect_null(res$breaks)
  expect_length(res$series, 0)
})

test_that("mdi_check_harmonized skips break detection without a year column", {
  dt <- make_phc_data()[, !"year"]
  res <- mdi_check_harmonized(dt, "SBS", make_phc_catalogue(), "FI")
  expect_null(res$breaks)
})

test_that("mdi_check_harmonized handles integer64 columns", {
  skip_if_not_installed("bit64")
  dt <- make_phc_data()
  dt[["emp"]] <- bit64::as.integer64(dt[["emp"]])
  res <- mdi_check_harmonized(dt, "SBS", make_phc_catalogue(), "FI")
  expect_equal(res$series$emp$mean, c(15, 15, 35))
})

test_that("mdi_check_harmonized validates its arguments", {
  expect_error(mdi_check_harmonized(data.frame(), "SBS", make_phc_catalogue(), "FI"), "data.table")
  expect_error(mdi_check_harmonized(make_phc_data(), "SBS", make_phc_catalogue(), "AT"), "AT")
  expect_error(mdi_check_harmonized(make_phc_data(), "SBS", make_phc_catalogue(), "FI",
                                    threshold = -1), "threshold")
})

test_that("mdi_plot_breaks returns a ggplot titled by dataset and variable", {
  skip_if_not_installed("ggplot2")
  # draw to no device, so no Rplots.pdf is left behind
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  res <- mdi_check_harmonized(make_phc_data(), "SBS", make_phc_catalogue(), "FI")
  p <- mdi_plot_breaks(res$series$emp, res$breaks)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Break Detection in Aggregates: SBS - emp")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("from a previous 0, quantiles aren't flagged and a mean or sd leaving 0 is marked from_zero", {
  dt <- data.table::data.table(year = rep(2019:2021, each = 4),
                               emp = c(0, 0, 0, 0,  0, 0, 0, 8,  0, 0, 0, 8))
  cat <- data.table::data.table(MD_dataset = "SBS", MD_varname = c("year", "emp"), is_key = c(1, 0),
                                class = c("integer", "numeric"), domain = "", FI = "")
  res <- mdi_check_harmonized(dt, "SBS", cat, "FI")
  b <- res$breaks
  expect_false(any(is.infinite(b$growth)))
  # 2020: mean and sd leave 0; p75 (0 -> 2) isn't flagged
  expect_setequal(b[year == 2020]$stat, c("mean", "sd"))
  expect_true(all(b[year == 2020]$from_zero))
  expect_true(all(is.na(b[year == 2020]$growth)))
  # 2021: unchanged, nothing flagged
  expect_equal(nrow(b[year == 2021]), 0)
})

test_that("classes are compared case-insensitively", {
  dt <- data.table::data.table(year = rep(2020:2021, each = 2), emp = c(10, 10, 30, 30))
  cat <- data.table::data.table(MD_dataset = "SBS", MD_varname = c("year", "emp"), is_key = c(1, 0),
                                class = c("integer", "Numeric"), domain = "", FI = "")
  res <- mdi_check_harmonized(dt, "SBS", cat, "FI")
  expect_equal(unique(res$breaks$variable), "emp")
})

test_that("the NA check flags jumps in the share of NAs and years with every value NA", {
  dt <- data.table::data.table(firmid = rep(c("a", "b", "c", "d"), 4), year = rep(2018:2021, each = 4),
                               exim = c(TRUE, FALSE, TRUE, FALSE,  TRUE, FALSE, TRUE, FALSE,
                                        TRUE, NA, TRUE, FALSE,  TRUE, NA, NA, FALSE),
                               exit = as.Date(NA), emp = c(1:4, 1:4, 1:4, 1:4))
  cat <- data.table::data.table(MD_dataset = "BR", MD_varname = c("firmid", "year", "exim", "exit", "emp"),
                                is_key = c(1, 1, 0, 0, 0),
                                class = c("character", "integer", "logical", "date", "numeric"),
                                domain = c("", "", "", "%d%m%Y", ""), FI = "")
  res <- mdi_check_harmonized(dt, "BR", cat, "FI")
  na <- res$na_shares
  # exim: 0% -> 25% (2020) -> 50% (2021), each +25 points
  expect_equal(na[variable == "exim"]$year, c(2020, 2021))
  expect_equal(na[variable == "exim"]$change, c(0.25, 0.25))
  # exit: NA in every year, flagged every year though the share doesn't change
  expect_equal(na[variable == "exit"]$year, 2018:2021)
  expect_true(all(na[variable == "exit"]$all_na))
  expect_false("emp" %in% na$variable)
  expect_false("firmid" %in% na$variable)
  expect_setequal(names(res$na_series), c("exim", "exit"))
  # a smaller jump than the threshold isn't flagged
  res <- mdi_check_harmonized(dt, "BR", cat, "FI", na_threshold = 0.3)
  expect_false("exim" %in% res$na_shares$variable)
})

test_that("mdi_plot_na_share returns a ggplot", {
  skip_if_not_installed("ggplot2")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  s <- data.table::data.table(year = 2019:2021, na_share = c(0, 0, 0.3))
  p <- mdi_plot_na_share(s, data.table::data.table(year = 2021), "BR - exim")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "BR - exim")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("by default a change of 20% isn't a break, with threshold = 0.1 it is", {
  dt <- data.table::data.table(year = rep(2020:2021, each = 2), emp = c(10, 10, 12, 12))
  cat <- data.table::data.table(MD_dataset = "SBS", MD_varname = c("year", "emp"), is_key = c(1, 0),
                                class = c("integer", "numeric"), domain = "", FI = "")
  expect_null(mdi_check_harmonized(dt, "SBS", cat, "FI")$breaks)
  expect_setequal(mdi_check_harmonized(dt, "SBS", cat, "FI", threshold = 0.1)$breaks$stat,
                  c("p25", "p50", "p75", "mean"))
})
