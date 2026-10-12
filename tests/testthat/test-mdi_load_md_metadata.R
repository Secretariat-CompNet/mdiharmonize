make_md_fixture <- function() {
  dir <- paste0(tempfile("md_"), "/")
  dir.create(dir, recursive = TRUE)
  writeLines(c("code,description", "1,Active", "2,Closed"), paste0(dir, "MD_codebook.csv"))
  writeLines(c("MD_varname,is_key,description", "firmid,1,ID"), paste0(dir, "MD_varnames.csv"))
  writeLines(c("MD_dataset,MD_varname,is_key", "BR,firmid,1"), paste0(dir, "MD_catalogue.csv"))
  writeLines(c("hier,h_0", "MD_nace_hier,nace"), paste0(dir, "MD_nace_hier.csv"))
  writeLines(c("name,description", "BR,Business Register"), paste0(dir, "MD_datafiles.csv"))
  writeLines(c("disclosure_variable,FI", "MDIminNumObs,3"), paste0(dir, "MD_disclosure_info.csv"))
  dir
}

test_that("mdi_load_md_metadata loads the classification/lookup files", {
  dir <- make_md_fixture()
  md <- mdi_load_md_metadata(dir)

  expect_true(all(c("MD_codebook", "MD_varnames", "MD_catalogue", "MD_nace_hier",
                     "MD_datafiles") %in% names(md)))
  expect_type(md$MD_codebook$code, "character")
  expect_type(md$MD_varnames$is_key, "integer")
  expect_type(md$MD_catalogue$is_key, "integer")
})

test_that("mdi_load_md_metadata excludes MD_disclosure_info.csv", {
  dir <- make_md_fixture()
  md <- mdi_load_md_metadata(dir)
  expect_null(md$MD_disclosure_info)
})

test_that("mdi_load_md_metadata reads MD_datafiles.csv without forcing character", {
  dir <- make_md_fixture()
  # add a numeric-looking column to prove it isn't touched by the char rule
  writeLines(c("name,year_start", "BR,2003"), paste0(dir, "MD_datafiles.csv"))
  md <- mdi_load_md_metadata(dir)
  expect_type(md$MD_datafiles$year_start, "integer")
})
