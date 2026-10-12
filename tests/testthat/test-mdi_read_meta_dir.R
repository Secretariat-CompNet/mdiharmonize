test_that("mdi_read_meta_dir forces non-excepted columns to character", {
  dir <- tempfile()
  dir.create(dir)
  writeLines(c("code,description,year", "1,Active,2020", "2,Closed,2021"),
             paste0(dir, "/MD_codebook.csv"))

  result <- mdi_read_meta_dir(dir, pattern = "_codebook\\.csv$")

  expect_named(result, "MD_codebook")
  expect_s3_class(result$MD_codebook, "data.table")
  expect_type(result$MD_codebook$code, "character")
  expect_type(result$MD_codebook$description, "character")
  expect_type(result$MD_codebook$year, "double")
  expect_equal(result$MD_codebook$year, c(2020, 2021))
})

test_that("mdi_read_meta_dir leaves char_except columns untouched", {
  dir <- tempfile()
  dir.create(dir)
  writeLines(c("MD_varname,is_key,description", "firmid,1,ID", "status,0,Status"),
             paste0(dir, "/MD_varnames.csv"))

  result <- mdi_read_meta_dir(dir, pattern = "_varnames\\.csv$")

  expect_type(result$MD_varnames$is_key, "integer")
  expect_type(result$MD_varnames$description, "character")
})

test_that("mdi_read_meta_dir matches a suffix across multiple files", {
  dir <- tempfile()
  dir.create(dir)
  writeLines(c("hier,h_0", "nace,4"), paste0(dir, "/MD_nace_hier.csv"))
  writeLines(c("hier,h_0", "nuts,3"), paste0(dir, "/MD_nuts_hier.csv"))
  writeLines(c("a,b", "1,2"), paste0(dir, "/MD_other.csv"))

  result <- mdi_read_meta_dir(dir, pattern = "_hier\\.csv$")

  expect_setequal(names(result), c("MD_nace_hier", "MD_nuts_hier"))
})

test_that("mdi_read_meta_dir returns an empty list when nothing matches", {
  dir <- tempfile()
  dir.create(dir)
  result <- mdi_read_meta_dir(dir, pattern = "_nonexistent\\.csv$")
  expect_length(result, 0)
  expect_true(is.list(result))
})

test_that("mdi_read_meta_dir validates its string arguments", {
  expect_error(mdi_read_meta_dir(1, "x"), "dir")
  expect_error(mdi_read_meta_dir("x", 1), "pattern")
})

test_that("mdi_read_meta_dir keeps leading zeros in codes", {
  dir <- tempfile()
  dir.create(dir)
  writeLines(c("h_0,h_1,h_2", "0111,011,01", "1011,101,10"),
             paste0(dir, "/MD_nace_hier.csv"))

  result <- mdi_read_meta_dir(dir, pattern = "_hier\\.csv$")

  expect_equal(result$MD_nace_hier$h_0, c("0111", "1011"))
  expect_equal(result$MD_nace_hier$h_2, c("01", "10"))
})

test_that("mdi_read_meta_dir keeps a literal NA code as text and reads empty cells as NA", {
  dir <- tempfile()
  dir.create(dir)
  writeLines(c("MD_dataset,MD_varname,code,description",
               "PRODCOM,unit,NA,Not available",
               "PRODCOM,unit,,"),
             paste0(dir, "/MD_codebook.csv"))

  result <- mdi_read_meta_dir(dir, pattern = "_codebook\\.csv$")

  expect_equal(result$MD_codebook$code, c("NA", NA))
})

test_that("mdi_read_plain_csv reads both empty and literal NA as missing", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("NSI_datafile,yearvar,year_start", "a,NA,2003", "b,,2004", "c,jahr,2005"), path)
  dt <- mdi_read_plain_csv(path)
  expect_equal(dt$yearvar, c(NA, NA, "jahr"))
  expect_type(dt$year_start, "integer")
})
