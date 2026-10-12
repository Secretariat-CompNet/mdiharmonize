make_read_cfg <- function(dir, preprocessing = NA_character_) {
  conc <- data.table::data.table(
    NSI_dataset = "sbs", NSI_varname = c("EMP", "NACE", "LFO", "OTHER"),
    MD_varname = c("emp", "nace", "lfo", "other"), method = c("remap", "remap", "recode", "remap"),
    detail = c(NA, NA, "FI_MD_codeconc", NA), year = c(2020, 2020, 2020, 2019))
  varnames <- data.table::data.table(
    NSI_datafile = "sbs_2020", NSI_varname = c("EMP", "NACE", "LFO", "ID", "BIRTH", "OTHER"),
    class = c("numeric", "character", "character", "character", "date", "numeric"))
  datafiles <- data.table::data.table(
    NSI_dataset = "sbs", NSI_datafile = "sbs_2020", yearvar = NA_character_, year_start = 2020,
    format = "csv", path = "", preprocessing = preprocessing, prepro_vars = list(character(0)))
  list(dirs = list(inputdata = dir), country = "FI", db_user = NULL,
       nsi = list(NSI_MD_conc = conc, NSI_varnames = varnames, NSI_datafiles = datafiles))
}

test_that(".import_spec reads the year's concordance variables, codes as text", {
  cfg <- make_read_cfg(tempdir())
  spec <- .import_spec(cfg$nsi$NSI_datafiles, cfg$nsi$NSI_MD_conc, cfg$nsi$NSI_varnames)
  # OTHER is only mapped for 2019
  expect_setequal(spec$cols, c("EMP", "NACE", "LFO"))
  expect_setequal(spec$char_cols, c("NACE", "LFO"))
})

test_that(".import_spec adds the year variable and the preprocessing's variables", {
  cfg <- make_read_cfg(tempdir(), preprocessing = "reshape|x|NACE|y")
  cfg$nsi$NSI_datafiles$yearvar <- "YEAR"
  cfg$nsi$NSI_datafiles$prepro_vars <- list("BIRTH")
  cfg$nsi$NSI_varnames <- rbind(cfg$nsi$NSI_varnames,
                                data.table::data.table(NSI_datafile = "sbs_2020", NSI_varname = "YEAR", class = "numeric"))
  spec <- .import_spec(cfg$nsi$NSI_datafiles, cfg$nsi$NSI_MD_conc, cfg$nsi$NSI_varnames)
  expect_setequal(spec$cols, c("EMP", "NACE", "LFO", "YEAR", "BIRTH"))
  # BIRTH is declared date; NACE is the reshape's names_from, listed again
  expect_true(all(c("BIRTH", "NACE", "LFO") %in% spec$char_cols))
})

test_that(".import_spec reads region codes and every reshape names_from column as text", {
  cfg <- make_read_cfg(tempdir(), preprocessing = "reshape|ID|NACE#EMP|OTHER")
  cfg$nsi$NSI_MD_conc$MD_varname[1] <- "lau"
  spec <- .import_spec(cfg$nsi$NSI_datafiles, cfg$nsi$NSI_MD_conc, cfg$nsi$NSI_varnames)
  # EMP is declared numeric: it is text as a lau code and as a names_from column
  expect_true("EMP" %in% spec$char_cols)
  cfg$nsi$NSI_MD_conc$MD_varname[1] <- "emp"
  spec <- .import_spec(cfg$nsi$NSI_datafiles, cfg$nsi$NSI_MD_conc, cfg$nsi$NSI_varnames)
  expect_true("EMP" %in% spec$char_cols)
})

test_that(".import_spec ignores concordance rows with a missing year", {
  cfg <- make_read_cfg(tempdir())
  cfg$nsi$NSI_MD_conc$year[4] <- NA
  spec <- .import_spec(cfg$nsi$NSI_datafiles, cfg$nsi$NSI_MD_conc, cfg$nsi$NSI_varnames)
  expect_false(anyNA(spec$cols))
})

test_that("mdi_read_nsi_datafile reads the needed columns, keeping leading zeros", {
  dir <- paste0(tempdir(), "/read_nsi/")
  dir.create(dir, showWarnings = FALSE)
  data.table::fwrite(data.table::data.table(ID = 1:2, EMP = c(3, 4), NACE = c("0111", "1011"),
                                            LFO = c("01", "02"), OTHER = 9),
                     paste0(dir, "sbs_2020.csv"))
  cfg <- make_read_cfg(dir)
  dt <- mdi_read_nsi_datafile(cfg, cfg$nsi$NSI_datafiles)
  expect_setequal(names(dt), c("EMP", "NACE", "LFO"))
  expect_equal(dt$NACE, c("0111", "1011"))
  expect_equal(dt$LFO, c("01", "02"))
})

test_that("a missing NSI data file is named, with the hint about the country folder", {
  dir <- paste0(tempfile("read_missing_"), "/")
  dir.create(dir)
  cfg <- make_read_cfg(dir)
  expect_error(mdi_read_nsi_datafile(cfg, cfg$nsi$NSI_datafiles),
               "NSI data file not found: .*sbs_2020\\.csv.*dirINPUTDATA must include the country")
})

test_that("mdi_read_nsi_datafile validates its arguments", {
  cfg <- make_read_cfg(tempdir())
  expect_error(mdi_read_nsi_datafile(cfg, rbind(cfg$nsi$NSI_datafiles, cfg$nsi$NSI_datafiles)), "one row")
  cfg$nsi <- NULL
  expect_error(mdi_read_nsi_datafile(cfg, data.table::data.table()), "NSI metadata")
})
