make_harmonize_conc <- function(...) {
  rows <- list(...)
  data.table::rbindlist(lapply(rows, function(r) {
    data.table::data.table(NSI_dataset = "sbs", NSI_varname = r[1], MD_varname = r[2],
                           method = r[3], detail = r[4], year = 2020)
  }))
}

test_that("mdi_harmonize revalues, remaps and sets year from the concordance", {
  dt <- data.table::data.table(TURN = c(1, 2), EMP = c(3, 4), other = 5:6)
  conc <- make_harmonize_conc(c("TURN", "turnover", "revalue", "x*1000"),
                              c("TURN", "turnover", "remap", NA),
                              c("EMP", "emp", "remap", NA))
  out <- suppressMessages(capture.output(res <- mdi_harmonize(dt, conc, "FI")))
  expect_equal(names(res), c("turnover", "emp", "year"))
  expect_equal(res$turnover, c(1000, 2000))
  expect_equal(res$year, c(2020L, 2020L))
})

test_that("mdi_harmonize recodes with a country codeconc, filtered to the variable and dataset", {
  dt <- data.table::data.table(LFO = c("1", "2", "9"))
  conc <- make_harmonize_conc(c("LFO", "lfo", "recode", "FI_MD_codeconc"),
                              c("LFO", "lfo", "remap", NA))
  codeconc <- data.table::data.table(
    NSI_dataset = c("sbs", "sbs", "br"), NSI_varname = c("LFO", "LFO", "LFO"),
    left = c("1", "2", "1"), right = c("A", "B", "Z"))
  capture.output(res <- mdi_harmonize(dt, conc, "FI", code_tables = list(NSI_MD_codeconc = codeconc)))
  # 9 has no code: kept as is
  expect_equal(as.character(res$lfo), c("A", "B", "9"))
})

test_that("mdi_harmonize warns once when a recode's table has no codes for it", {
  dt <- data.table::data.table(LFO = c("1", "2"), EXIM = c("1", "2"))
  conc <- make_harmonize_conc(c("LFO", "lfo", "recode", "FI_MD_codeconc"),
                              c("EXIM", "exim", "recode", "FI_MD_codeconc"),
                              c("LFO", "lfo", "remap", NA), c("EXIM", "exim", "remap", NA))
  # LFO's codes are there, but EXIM's only under another dataset name, and
  # for years other than this file's
  codeconc <- data.table::data.table(
    NSI_dataset = c("sbs", "sbs", "sbs_2019", "sbs", "sbs"), year = c(2020, 2020, 2020, 2018, 2019),
    NSI_varname = c("LFO", "LFO", "EXIM", "EXIM", "EXIM"),
    left = c("1", "2", "1", "1", "1"), right = c("A", "B", "0", "0", "0"))
  expect_warning(capture.output(res <- mdi_harmonize(dt, conc, "FI", code_tables = list(NSI_MD_codeconc = codeconc))),
                 "no codes in the code table for EXIM \\(NSI dataset sbs, year 2020, table FI_MD_codeconc\\)\\. Their values")
  expect_equal(as.character(res$lfo), c("A", "B"))
  expect_equal(as.character(res$exim), c("1", "2"))
})

test_that("mdi_harmonize reclasses with an MD-level classconc", {
  dt <- data.table::data.table(CTRY = c("040", "246"))
  conc <- make_harmonize_conc(c("CTRY", "ctry", "reclass", "MD_iso_classconc"),
                              c("CTRY", "ctry", "remap", NA))
  tables <- list(MD_iso_classconc = data.table::data.table(left = c("040", "246"), right = c("AT", "FI")))
  capture.output(res <- mdi_harmonize(dt, conc, "FI", code_tables = tables))
  expect_equal(as.character(res$ctry), c("AT", "FI"))
})

test_that("mdi_harmonize says which code table is missing", {
  dt <- data.table::data.table(HO = "1")
  conc <- make_harmonize_conc(c("HO", "ho", "reclass", "FR_HO_MD_ho_classconc"))
  tables <- list(NSI_Ho_MD_ho_classconc = data.table::data.table(left = "1", right = "a"))
  expect_error(capture.output(mdi_harmonize(dt, conc, "FR", code_tables = tables)),
               "FR_HO_MD_ho_classconc.*NSI_HO_MD_ho_classconc")
  conc$detail <- NA_character_
  expect_error(capture.output(mdi_harmonize(dt, conc, "FR", code_tables = tables)),
               "doesn't name a code table")
})

test_that("mdi_harmonize redefines an MD variable as an NA-safe sum", {
  dt <- data.table::data.table(A = c(1, NA, NA), B = c(2, 3, NA))
  conc <- make_harmonize_conc(c("A", "total", "redefine", "+"), c("B", "total", "redefine", "+"))
  capture.output(res <- mdi_harmonize(dt, conc, "FI"))
  expect_equal(res$total, c(3, 3, NA))
})

test_that("mdi_harmonize redefine applies each row's sign", {
  dt <- data.table::data.table(A = c(10, NA, NA), B = c(2, 3, NA), C = c(1, NA, NA))
  conc <- make_harmonize_conc(c("A", "net", "redefine", "+"), c("B", "net", "redefine", "-"),
                              c("C", "net", "redefine", "-"))
  capture.output(res <- mdi_harmonize(dt, conc, "FI"))
  expect_equal(res$net, c(7, -3, NA))
  # the first row's sign doesn't negate its variable: "-A -B" = A - B
  conc <- make_harmonize_conc(c("A", "net", "redefine", "-"), c("B", "net", "redefine", "-"))
  capture.output(res <- mdi_harmonize(dt[, c("A", "B")], conc, "FI"))
  expect_equal(res$net, c(8, -3, NA))
})

test_that("mdi_harmonize takes year from a panel's year variable", {
  dt <- data.table::data.table(JAHR = c("2019", "2020"), EMP = 1:2)
  conc <- make_harmonize_conc(c("EMP", "emp", "remap", NA))
  capture.output(res <- mdi_harmonize(dt, conc, "AT", year_var = "JAHR"))
  expect_equal(names(res), c("year", "emp"))
  expect_equal(res$year, c(2019L, 2020L))
})

test_that("mdi_harmonize keeps a remapped year variable once", {
  dt <- data.table::data.table(JAHR = c("2019", "2020"), EMP = 1:2)
  conc <- make_harmonize_conc(c("JAHR", "year", "remap", NA), c("EMP", "emp", "remap", NA))
  capture.output(res <- mdi_harmonize(dt, conc, "AT", year_var = "JAHR"))
  expect_equal(sort(names(res)), c("emp", "year"))
  expect_equal(res$year, c(2019L, 2020L))
})

test_that("mdi_harmonize validates its arguments", {
  dt <- data.table::data.table(EMP = 1)
  conc <- make_harmonize_conc(c("EMP", "emp", "remap", NA))
  expect_error(mdi_harmonize(dt, conc[, !"year"], "FI"), "year")
  expect_error(mdi_harmonize(dt, conc, NULL), "country")
  expect_error(mdi_harmonize(dt, conc, "FI", code_tables = "x"), "code_tables")
  expect_error(mdi_harmonize(dt, conc, "FI", year_var = c("a", "b")), "year_var")
})

test_that("mdi_harmonize stops and names concordance variables missing from the data", {
  dt <- data.table::data.table(ID = 1:2, K = c("a", "b"))
  conc <- data.table::data.table(NSI_dataset = "s", NSI_varname = c("ID", "NOPE"), MD_varname = c("firmid", "x"),
                                 method = c("remap", "recode"), detail = c(NA, "NSI_t_codebook"), year = 2020)
  # recode used to create an all-NA column silently
  expect_error(mdi_harmonize(dt, conc, "FI", code_tables = list(NSI_t_codebook = data.table::data.table(left = "a", right = "A"))),
               "maps variable\\(s\\) NOPE, which are not in the data \\(NSI dataset s, year 2020\\)")
})
