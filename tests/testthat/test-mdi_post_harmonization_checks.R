make_report_cfg <- function() {
  dir <- paste0(tempdir(), "/phc_report/")
  dir.create(dir, showWarnings = FALSE)
  unlink(list.files(dir, full.names = TRUE))
  saveRDS(data.table::data.table(firmid = c("a", "a", "b"), year = c(2020L, 2020L, NA), emp = c(1, 1, 1)),
          paste0(dir, "SBS.RDS"))
  catalogue <- data.table::data.table(MD_dataset = "SBS", MD_varname = c("firmid", "year", "emp"),
                                      is_key = c(1L, 1L, 0L), class = c("character", "integer", "numeric"),
                                      domain = "", FI = "2020 - 2021")
  list(dirs = list(tmpsave = dir), country = "FI",
       md = list(MD_catalogue = catalogue, MD_varnames = catalogue[, c("MD_dataset", "MD_varname")]),
       nsi = list(NSI_MD_conc = data.table::data.table(MD_dataset = "SBS", MD_varname = c("firmid", "emp"))))
}

test_that("mdi_post_harmonization_checks writes the report, flagging what's wrong", {
  cfg <- make_report_cfg()
  capture.output(res <- mdi_post_harmonization_checks(cfg))
  txt <- readLines(file.path(cfg$dirs$tmpsave, "FI_phc_results.txt"))
  expect_true(any(grepl("Duplicate Check Summary", txt)))
  expect_true(any(grepl("No coercion_fallout_log.csv found", txt)))
  expect_true(any(grepl("3 variables checked, all classes match", txt)))
  expect_true(res$SBS$duplicates$has_duplicates)
  expect_equal(res$SBS$years$num_rows_no_year, 1L)
  # the year check lists SBS
  year_section <- txt[seq(grep("Year Check", txt), grep("Variable Class", txt))]
  expect_true(any(grepl("SBS", year_section)))
})

test_that("mdi_post_harmonization_checks skips datasets that weren't harmonized", {
  cfg <- make_report_cfg()
  cfg$md$MD_catalogue <- rbind(cfg$md$MD_catalogue,
                               data.table::data.table(MD_dataset = "BR", MD_varname = "firmid", is_key = 1L,
                                                      class = "character", domain = "", FI = ""))
  cfg$md$MD_varnames <- cfg$md$MD_catalogue[, c("MD_dataset", "MD_varname")]
  capture.output(res <- mdi_post_harmonization_checks(cfg))
  expect_equal(names(res), "SBS")
})

test_that("mdi_post_harmonization_checks lists only the variables whose class doesn't match", {
  cfg <- make_report_cfg()
  cfg$md$MD_catalogue$class[3] <- "integer"
  capture.output(mdi_post_harmonization_checks(cfg))
  txt <- readLines(file.path(cfg$dirs$tmpsave, "FI_phc_results.txt"))
  class_section <- txt[seq(grep("Variable Class", txt), grep("Coercion Fallout", txt))]
  expect_true(any(grepl("emp", class_section)))
  expect_false(any(grepl("firmid", class_section)))
})

test_that("mdi_post_harmonization_checks names factor variables left unconverted on purpose, not as mismatches", {
  cfg <- make_report_cfg()
  cfg$md$MD_catalogue$class[1] <- "factor"
  cfg$md$MD_catalogue$domain[1] <- "MD_iso3166-2_class"
  capture.output(mdi_post_harmonization_checks(cfg))
  txt <- readLines(file.path(cfg$dirs$tmpsave, "FI_phc_results.txt"))
  expect_true(any(grepl("all classes match apart from the ones below", txt)))
  expect_true(any(grepl("left as they are on purpose.*SBS firmid", txt)))
})

test_that("mdi_post_harmonization_checks checks computed variables and reports NA jumps, with a page each", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  cfg <- make_report_cfg()
  saveRDS(data.table::data.table(firmid = rep(c("a", "b"), 3), year = rep(2019:2021, each = 2),
                                 emp = c(1, 2, 1, 2, NA, NA), K_pim = c(10, 10, 10, 10, 50, 50)),
          file.path(cfg$dirs$tmpsave, "SBS.RDS"))
  cfg$md$MD_catalogue <- rbind(cfg$md$MD_catalogue,
                               data.table::data.table(MD_dataset = "SBS", MD_varname = "K_pim", is_key = 0L,
                                                      class = "numeric", domain = "", FI = ""))
  cfg$md$MD_varnames <- cfg$md$MD_catalogue[, c("MD_dataset", "MD_varname")]
  # K_pim isn't in the concordance: computed by the pipeline
  capture.output(res <- mdi_post_harmonization_checks(cfg))
  expect_true("K_pim" %in% res$SBS$breaks$variable)
  expect_equal(res$SBS$na_shares[variable == "emp"]$year, 2021)
  txt <- readLines(file.path(cfg$dirs$tmpsave, "FI_phc_results.txt"))
  expect_true(any(grepl("NA Share Check", txt)))
  expect_true(file.exists(file.path(cfg$dirs$tmpsave, "breaks_report.pdf")))
})

test_that("a variable NA in every year is listed on one line, not a row per year", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  cfg <- make_report_cfg()
  saveRDS(data.table::data.table(firmid = rep(c("a", "b"), 3), year = rep(2019:2021, each = 2),
                                 emp = NA_real_),
          file.path(cfg$dirs$tmpsave, "SBS.RDS"))
  capture.output(mdi_post_harmonization_checks(cfg))
  txt <- readLines(file.path(cfg$dirs$tmpsave, "FI_phc_results.txt"))
  section <- txt[grep("NA Share Check", txt):length(txt)]
  expect_true(any(grepl("NA in every year: SBS emp.", section, fixed = TRUE)))
  expect_false(any(grepl("2020", section)))
})
