make_deflators_fixture <- function() {
  rocket_dir <- paste0(tempfile("rocket_"), "/")
  mdmeta_dir <- paste0(tempfile("mdmeta_"), "/")
  dir.create(paste0(rocket_dir, "auxdata"), recursive = TRUE)
  dir.create(mdmeta_dir, recursive = TRUE)

  writeLines(c(
    "cc,year,DEFind,pnv",
    "FI,1995,A,1.1",
    "YY,1995,A,2.2",
    "YYx,1995,A,2.2"
  ), paste0(rocket_dir, "auxdata/deflators.csv"))

  writeLines(c("nace,DEFind", "\"0111\",A01"), paste0(mdmeta_dir, "MD_nace_DEFind_conc.csv"))

  list(rocket_dir = rocket_dir, mdmeta_dir = mdmeta_dir)
}

test_that("mdi_load_deflators filters to the given country and coerces measurement columns to numeric", {
  f <- make_deflators_fixture()
  result <- mdi_load_deflators(f$rocket_dir, f$mdmeta_dir, "FI")

  expect_equal(result$deflators$cc, "FI")
  expect_type(result$deflators$pnv, "double")
  expect_type(result$deflators$year, "double")
})

test_that("mdi_load_deflators is country-agnostic: a variant code reads its own row, not the country's", {
  f <- make_deflators_fixture()

  variant <- mdi_load_deflators(f$rocket_dir, f$mdmeta_dir, "YYx")
  expect_equal(variant$deflators$cc, "YYx")
  expect_equal(nrow(variant$deflators), 1)
})

test_that("mdi_load_deflators keeps nace as character in the concordance", {
  f <- make_deflators_fixture()
  result <- mdi_load_deflators(f$rocket_dir, f$mdmeta_dir, "FI")
  expect_type(result$nace_DEFind$nace, "character")
  expect_equal(result$nace_DEFind$nace, "0111")
})

test_that("mdi_load_deflators warns when the country has no deflators", {
  f <- make_deflators_fixture()
  expect_warning(res <- mdi_load_deflators(f$rocket_dir, f$mdmeta_dir, "XX"), "no rows for XX")
  expect_equal(nrow(res$deflators), 0)
})
