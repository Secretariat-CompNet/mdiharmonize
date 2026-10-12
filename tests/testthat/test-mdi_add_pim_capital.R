make_pim_cfg <- function(rocket_dir = tempdir()) {
  list(
    dirs = list(rocket = rocket_dir, tmpsave = tempdir(), nsimeta = tempdir()), country = "FI",
    md = list(MD_idInfo = data.table::data.table(MD_dataset = c("BR", "SBS"), FI = "firmid")),
    nsi = list(NSI_MD_conc = data.table::data.table(
      MD_dataset = c("BR", "BR", "SBS", "SBS", "SBS"),
      MD_varname = c("firmid", "nace", "firmid", "ni_tan", "persons")))
  )
}

test_that("mdi_add_pim_capital skips without the capital stocks file", {
  cfg <- make_pim_cfg(file.path(tempdir(), "no_rocket"))
  expect_message(done <- mdi_add_pim_capital(cfg), "capital_stocks_FI.csv not found")
  expect_false(done)
})

test_that("mdi_add_pim_capital says which MD variables are missing", {
  cfg <- make_pim_cfg(file.path(tempdir(), "no_rocket"))
  cfg$nsi$NSI_MD_conc <- cfg$nsi$NSI_MD_conc[cfg$nsi$NSI_MD_conc$MD_varname != "ni_tan"]
  expect_message(mdi_add_pim_capital(cfg), "SBS\\(ni_tan\\)")
})

test_that("mdi_add_pim_capital skips a country without an ID column", {
  cfg <- make_pim_cfg(file.path(tempdir(), "no_rocket"))
  cfg$country <- "XX"
  expect_message(done <- suppressMessages(mdi_add_pim_capital(cfg)), NA)
  expect_message(mdi_add_pim_capital(cfg), "not found in MD_idInfo")
  expect_false(done)
})

test_that("mdi_add_pim_capital skips when SBS or BR isn't harmonized yet", {
  rocket <- file.path(tempfile("rocket_"), "")
  dir.create(file.path(rocket, "auxdata"), recursive = TRUE)
  file.create(file.path(rocket, "auxdata", "capital_stocks_FI.csv"))
  cfg <- make_pim_cfg(rocket)
  cfg$dirs$tmpsave <- file.path(tempfile("tmp_"), "")
  dir.create(cfg$dirs$tmpsave)
  saveRDS(data.table::data.table(), file.path(cfg$dirs$tmpsave, "SBS.RDS"))
  expect_message(done <- mdi_add_pim_capital(cfg), "BR.RDS not in dirTMPSAVE")
  expect_false(done)
})
