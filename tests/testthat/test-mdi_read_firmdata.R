# Two NSI datasets: a register (reg) feeding BR and SBS, and a survey (svy)
# feeding SBS. BR is built first, so reg is read once, kept (trimmed to what
# SBS needs), and dropped after SBS.
make_firmdata_cfg <- function() {
  input <- paste0(tempdir(), "/firmdata_in/")
  out <- paste0(tempdir(), "/firmdata_out/")
  dir.create(input, showWarnings = FALSE)
  dir.create(out, showWarnings = FALSE)
  unlink(list.files(out, full.names = TRUE))
  data.table::fwrite(data.table::data.table(ID = c("a", "b"), NACE = c("0111", "1011"), EMP = c("3", "x")),
                     paste0(input, "reg_2020.csv"))
  data.table::fwrite(data.table::data.table(ID = c("b", "c"), TURN = c(10, 20)),
                     paste0(input, "svy_2020.csv"))

  conc <- data.table::data.table(
    NSI_dataset = c("reg", "reg", "reg", "reg", "svy", "svy"),
    NSI_varname = c("ID", "NACE", "ID", "EMP", "ID", "TURN"),
    MD_dataset = c("BR", "BR", "SBS", "SBS", "SBS", "SBS"),
    MD_varname = c("firmid", "nace", "firmid", "emp", "firmid", "turnover"),
    method = "remap", detail = NA_character_, year = 2020)
  varnames <- data.table::data.table(
    NSI_datafile = c("reg_2020", "reg_2020", "reg_2020", "svy_2020", "svy_2020"),
    NSI_varname = c("ID", "NACE", "EMP", "ID", "TURN"),
    class = c("character", "character", "numeric", "character", "numeric"))
  datafiles <- data.table::data.table(
    NSI_dataset = c("reg", "svy"), NSI_datafile = c("reg_2020", "svy_2020"),
    yearvar = NA_character_, year_start = 2020, format = "csv", path = "",
    preprocessing = NA_character_, prepro_vars = list(character(0), character(0)))
  md_varnames <- data.table::data.table(
    MD_dataset = c("BR", "BR", "BR", "SBS", "SBS", "SBS", "SBS"),
    MD_varname = c("firmid", "year", "nace", "firmid", "year", "emp", "turnover"),
    is_key = c(1L, 1L, 0L, 1L, 1L, 0L, 0L),
    class = c("character", "integer", "character", "character", "integer", "numeric", "numeric"),
    domain = NA_character_)
  list(
    dirs = list(inputdata = input, tmpsave = out), country = "FI", db_user = NULL,
    flags = list(debug = FALSE),
    md = list(MD_varnames = md_varnames, MD_idInfo = data.table::data.table(MD_dataset = c("BR", "SBS"), FI = "firmid")),
    nsi = list(NSI_MD_conc = conc, NSI_varnames = varnames, NSI_datafiles = datafiles),
    conctype = data.table::data.table(NSI_dataset = c("reg", "reg", "svy"), MD_dataset = c("BR", "SBS", "SBS"),
                                      openNSI = c(1, 0, 1), closeNSI = c(0, 1, 1))
  )
}

test_that("mdi_read_firmdata builds and saves each MD dataset", {
  cfg <- make_firmdata_cfg()
  capture.output(log <- suppressWarnings(mdi_read_firmdata(cfg, cfg$conctype)))

  br <- readRDS(file.path(cfg$dirs$tmpsave, "BR.RDS"))
  expect_equal(br$nace, c("0111", "1011"))
  expect_equal(br$year, c(2020L, 2020L))
  expect_equal(data.table::key(br), c("firmid", "year"))

  # merged on firmid and year, all rows kept; emp converted to numeric
  sbs <- readRDS(file.path(cfg$dirs$tmpsave, "SBS.RDS"))
  expect_equal(sbs$firmid, c("a", "b", "c"))
  expect_equal(sbs$emp, c(3, NA, NA))
  expect_equal(sbs$turnover, c(NA, 10, 20))
})

test_that("mdi_read_firmdata names the file and variables when a mapped variable isn't read", {
  cfg <- make_firmdata_cfg()
  # mapped in the concordance, but not listed for the file, so never imported
  cfg$nsi$NSI_varnames <- cfg$nsi$NSI_varnames[NSI_varname != "NACE"]
  expect_error(capture.output(suppressWarnings(mdi_read_firmdata(cfg, cfg$conctype))),
               "NSI datafile 'reg_2020' \\(reg, 2020\\) lacks variable\\(s\\) NACE")
})

test_that("mdi_read_firmdata saves the conversion log", {
  cfg <- make_firmdata_cfg()
  capture.output(log <- suppressWarnings(mdi_read_firmdata(cfg, cfg$conctype)))
  saved <- data.table::fread(file.path(cfg$dirs$tmpsave, "coercion_fallout_log.csv"))
  expect_equal(saved$MD_varname, log$MD_varname)
  emp <- log[log$MD_varname == "emp"]
  expect_equal(emp$na_introduced, 1L)
})

test_that("with MDIdatasets, mdi_read_firmdata replaces only the chosen datasets' rows of the log", {
  cfg <- make_firmdata_cfg()
  data.table::fwrite(data.table::data.table(MD_dataset = c("BR", "ITGS"), MD_varname = c("old_br", "old_itgs"),
                                            desired_class = "numeric", na_before = 0L, na_after = 0L,
                                            na_introduced = 0L, logical_out_of_domain = 0L),
                     file.path(cfg$dirs$tmpsave, "coercion_fallout_log.csv"))
  cfg$datasets <- "BR"
  cfg$conctype <- cfg$conctype[MD_dataset == "BR"][, closeNSI := 1]
  capture.output(suppressWarnings(mdi_read_firmdata(cfg, cfg$conctype)))
  saved <- data.table::fread(file.path(cfg$dirs$tmpsave, "coercion_fallout_log.csv"))
  # ITGS's row from the earlier run stays, BR's old row is gone
  expect_true("old_itgs" %in% saved$MD_varname)
  expect_false("old_br" %in% saved$MD_varname)
  expect_false(file.exists(file.path(cfg$dirs$tmpsave, "SBS.RDS")))
})

test_that(".keep_for_later keeps what a file has, leaving a missing variable to the clear error later", {
  loaded <- list(f = data.table::data.table(ID = 1, A = 2))
  files <- data.table::data.table(NSI_datafile = "f", yearvar = NA_character_, year_start = 2020)
  conctype <- data.table::data.table(NSI_dataset = "n", MD_dataset = c("BR", "SBS"))
  conc <- data.table::data.table(NSI_dataset = "n", MD_dataset = "SBS", NSI_varname = c("ID", "NOPE"), year = 2020)
  out <- .keep_for_later(loaded, files, "n", FALSE, "BR", c("BR", "SBS"), conctype, conc)
  expect_equal(names(out$f), "ID")
})

test_that(".keep_for_later trims a kept file to what later datasets need, and drops a closed one", {
  cfg <- make_firmdata_cfg()
  loaded <- list(reg_2020 = data.table::data.table(ID = "a", NACE = "01", EMP = "3"))
  files <- data.table::data.table(NSI_datafile = "reg_2020", yearvar = NA_character_, year_start = 2020)
  kept <- .keep_for_later(loaded, files, "reg", FALSE, "BR", c("BR", "SBS"), cfg$conctype, cfg$nsi$NSI_MD_conc)
  expect_equal(names(kept$reg_2020), c("ID", "EMP"))
  expect_null(.keep_for_later(loaded, files, "reg", TRUE, "SBS", c("BR", "SBS"), cfg$conctype,
                              cfg$nsi$NSI_MD_conc)$reg_2020)
})

test_that(".md_sort_keys puts the firm id first and skips unmapped keys", {
  md_varnames <- data.table::data.table(MD_dataset = "ITGS", MD_varname = c("year", "cn08", "sh06", "firmid"),
                                        is_key = 1L)
  dt <- data.table::data.table(firmid = "a", year = 2020, cn08 = "01")
  expect_message(keys <- .md_sort_keys(dt, md_varnames, "ITGS", "FI"), "sh06")
  expect_equal(keys, c("firmid", "year", "cn08"))
})
