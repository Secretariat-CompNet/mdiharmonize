make_nsi_fixture <- function(country = "FI") {
  nsimeta_dir <- paste0(tempfile("nsimeta_"), "/")
  mdmeta_dir <- paste0(tempfile("mdmeta_"), "/")
  dir.create(nsimeta_dir, recursive = TRUE)
  dir.create(paste0(mdmeta_dir, country), recursive = TRUE)

  writeLines(c("NSI_datafile,path,year_start,year_end",
               "bd2003,bd/,2003,2003"),
             paste0(nsimeta_dir, country, "_datafiles.csv"))
  writeLines(c("NSI_datafile,NSI_varname,is_key", "bd2003,firmid,1"),
             paste0(nsimeta_dir, country, "_varnames.csv"))
  writeLines(c("NSI_dataset,NSI_varname,MD_varname,method,detail,year",
               "bd,acq,acq,revalue,as.logical(x),2003"),
             paste0(mdmeta_dir, country, "/", country, "_MD_conc.csv"))
  writeLines(c("code,description", "1,foo"),
             paste0(nsimeta_dir, country, "_class.csv"))

  list(nsimeta_dir = nsimeta_dir, mdmeta_dir = mdmeta_dir)
}

test_that("mdi_load_nsi_metadata loads and renames all NSI-side files", {
  f <- make_nsi_fixture("FI")
  nsi <- mdi_load_nsi_metadata(f$nsimeta_dir, f$mdmeta_dir, "FI")

  expect_true(all(c("NSI_datafiles", "NSI_varnames", "NSI_MD_conc",
                     "NSI_class") %in% names(nsi)))
  expect_false(any(grepl("^FI_", names(nsi))))
})

test_that("mdi_load_nsi_metadata keeps NSI_datafiles' year_start/year_end numeric", {
  f <- make_nsi_fixture("FI")
  nsi <- mdi_load_nsi_metadata(f$nsimeta_dir, f$mdmeta_dir, "FI")
  expect_type(nsi$NSI_datafiles$year_start, "integer")
  expect_type(nsi$NSI_datafiles$year_end, "integer")
})

test_that("mdi_load_nsi_metadata coerces the concordance's year to numeric and codes to character", {
  f <- make_nsi_fixture("FI")
  nsi <- mdi_load_nsi_metadata(f$nsimeta_dir, f$mdmeta_dir, "FI")
  expect_type(nsi$NSI_MD_conc$year, "double")
  expect_type(nsi$NSI_MD_conc$MD_varname, "character")
})

test_that("mdi_load_nsi_metadata leaves NSI_datafiles$path as declared", {
  # a country's mock data folder is part of dirINPUTDATA, set in the countdown
  f <- make_nsi_fixture("FI")
  nsi <- mdi_load_nsi_metadata(f$nsimeta_dir, f$mdmeta_dir, "FI")
  expect_equal(nsi$NSI_datafiles$path, "bd/")
})

test_that("mdi_load_nsi_metadata loads per-classification files under the names mdi_harmonize() looks up", {
  f <- make_nsi_fixture("FI")
  writeLines(c("code,description", "01011000,Horses"),
             paste0(f$nsimeta_dir, "FI_cn08_class.csv"))
  writeLines(c("left,right,year", "01011000,01012100,2008"),
             paste0(f$mdmeta_dir, "FI/FI_NSI_cn08_MD_cn08_classconc.csv"))

  nsi <- mdi_load_nsi_metadata(f$nsimeta_dir, f$mdmeta_dir, "FI")

  expect_true(all(c("NSI_cn08_class", "NSI_NSI_cn08_MD_cn08_classconc") %in% names(nsi)))
  # mdi_harmonize() swaps the leading country code for NSI in the detail
  expect_true(sub("FI", "NSI", "FI_NSI_cn08_MD_cn08_classconc") %in% names(nsi))
  expect_equal(nsi$NSI_cn08_class$code, "01011000")
  expect_equal(nsi$NSI_NSI_cn08_MD_cn08_classconc$left, "01011000")
  expect_equal(nsi$NSI_NSI_cn08_MD_cn08_classconc$year, 2008)
})
