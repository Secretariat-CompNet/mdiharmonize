make_sum_stat_dt <- function() {
  dt <- data.table::data.table(
    nace = c("A", "B"),
    sum_emp = c(100, 5),
    domPerc_emp = c(0.5, 0.5),
    NumObs = c(10L, 2L)
  )
  data.table::setattr(dt, "sumvars", "emp")
  data.table::setkeyv(dt, "nace")
  dt
}

make_reg_tab_dt <- function() {
  data.table::data.table(coef = c("a", "b"), Estimate = c(1, 2), df = c(20L, 2L), NumObs = c(25L, 25L))
}

test_that("mdi_export writes a sum_stat CSV with disclosure applied", {
  tmp_dir <- paste0(tempdir(), "/mdi_export_sumstat/")
  dir.create(tmp_dir, showWarnings = FALSE)

  mdi_export(
    DT = make_sum_stat_dt(), format = "csv", output_name = "test_out",
    datasets_used = "BR", purpose = "p", share_0_1 = "none", zeroes = "none",
    selection = "full", export_type = "sum_stat", CountryCode = "XX",
    res_group = "TST", dirOUTPUT = tmp_dir, minNumObs = 5
  )

  result <- data.table::fread(paste0(tmp_dir, "XX_TST_test_out.csv"))
  expect_equal(result[nace == "A"]$sum_emp, 100)
  expect_equal(result[nace == "B"]$sum_emp, -999)  # NumObs = 2 < minNumObs = 5

  desc <- readLines(paste0(tmp_dir, "OutputDescription.csv"))
  expect_true(any(grepl("test_out", desc)))
  expect_true(any(grepl("\"emp\"", desc)))
})

test_that("mdi_export's show_domPerc = 0 drops dominance columns from the output", {
  tmp_dir <- paste0(tempdir(), "/mdi_export_domperc/")
  dir.create(tmp_dir, showWarnings = FALSE)

  mdi_export(
    DT = make_sum_stat_dt(), format = "csv", output_name = "test_out",
    datasets_used = "BR", purpose = "p", share_0_1 = "none", zeroes = "none",
    selection = "full", export_type = "sum_stat", CountryCode = "XX",
    res_group = "TST", dirOUTPUT = tmp_dir, minNumObs = 5, show_domPerc = 0
  )

  result <- data.table::fread(paste0(tmp_dir, "XX_TST_test_out.csv"))
  expect_false("domPerc_emp" %in% names(result))
})

test_that("mdi_export's export_type = 'other' skips disclosure entirely", {
  tmp_dir <- paste0(tempdir(), "/mdi_export_other/")
  dir.create(tmp_dir, showWarnings = FALSE)

  mdi_export(
    DT = make_sum_stat_dt(), format = "csv", output_name = "test_other",
    datasets_used = "BR", purpose = "p", share_0_1 = "none", zeroes = "none",
    selection = "full", export_type = "other", CountryCode = "XX",
    res_group = "TST", dirOUTPUT = tmp_dir
  )

  result <- data.table::fread(paste0(tmp_dir, "XX_TST_test_other.csv"))
  expect_equal(result[nace == "B"]$sum_emp, 5)  # untouched: no disclosure for "other"
})

test_that("mdi_export's reg_tab path runs mdi_disclose_reg_tab and messages on redaction", {
  tmp_dir <- paste0(tempdir(), "/mdi_export_regtab/")
  dir.create(tmp_dir, showWarnings = FALSE)

  expect_message(
    mdi_export(
      DT = make_reg_tab_dt(), format = "csv", output_name = "test_reg",
      datasets_used = "BR", purpose = "p", share_0_1 = "none", zeroes = "none",
      selection = "full", export_type = "reg_tab", CountryCode = "XX",
      res_group = "TST", dirOUTPUT = tmp_dir, minNumObs = 3
    ),
    "masked due to failing disclosure"
  )

  result <- data.table::fread(paste0(tmp_dir, "XX_TST_test_reg.csv"))
  expect_equal(result[coef == "a"]$Estimate, 1)
  expect_true(is.na(result[coef == "b"]$Estimate))
})

test_that("mdi_export requires dirOUTPUT", {
  expect_error(
    mdi_export(
      DT = make_sum_stat_dt(), format = "csv", output_name = "x",
      datasets_used = "BR", purpose = "p", share_0_1 = "none", zeroes = "none",
      selection = "full", export_type = "sum_stat"
    ),
    "dirOUTPUT"
  )
})

test_that("mdi_export validates format and export_type against their allowed choices", {
  tmp_dir <- paste0(tempdir(), "/mdi_export_choices/")
  dir.create(tmp_dir, showWarnings = FALSE)

  expect_error(
    mdi_export(
      DT = make_sum_stat_dt(), format = "not_a_format", output_name = "x",
      datasets_used = "BR", purpose = "p", share_0_1 = "none", zeroes = "none",
      selection = "full", export_type = "sum_stat", dirOUTPUT = tmp_dir
    )
  )
  expect_error(
    mdi_export(
      DT = make_sum_stat_dt(), format = "csv", output_name = "x",
      datasets_used = "BR", purpose = "p", share_0_1 = "none", zeroes = "none",
      selection = "full", export_type = "not_a_type", dirOUTPUT = tmp_dir
    )
  )
})

test_that("mdi_export leaves the caller's table unchanged", {
  tmp_dir <- paste0(tempdir(), "/mdi_export_unchanged/")
  dir.create(tmp_dir, showWarnings = FALSE)
  for (show_values in c(0, 1)) {
    dt <- make_sum_stat_dt()
    before <- data.table::copy(dt)
    mdi_export(
      DT = dt, format = "csv", output_name = "test_out",
      datasets_used = "BR", purpose = "p", share_0_1 = "none", zeroes = "none",
      selection = "full", export_type = "sum_stat", CountryCode = "XX",
      res_group = "TST", dirOUTPUT = tmp_dir, minNumObs = 5, show_domPerc = 0,
      show_values = show_values
    )
    expect_identical(dt, before)
  }
})
