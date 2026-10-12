make_disclosure_fixture <- function() {
  dir <- paste0(tempfile("mdmeta_"), "/")
  dir.create(dir, recursive = TRUE)
  writeLines(c(
    "disclosure_variable,FI,DE",
    "MDIminNumObs,3,10",
    "MDIdomVar,var,persons_br",
    "MDIdomSh,0.75,0.5"
  ), paste0(dir, "MD_disclosure_info.csv"))
  dir
}

test_that("mdi_disclosure_config returns one named element per row, for the given country", {
  dir <- make_disclosure_fixture()
  disc <- mdi_disclosure_config(dir, "FI")

  expect_equal(disc, list(MDIminNumObs = 3, MDIdomVar = "var", MDIdomSh = 0.75))
})

test_that("mdi_disclosure_config reads a different country's own column", {
  dir <- make_disclosure_fixture()
  disc <- mdi_disclosure_config(dir, "DE")

  expect_equal(disc$MDIminNumObs, 10)
  expect_equal(disc$MDIdomVar, "persons_br")
})

test_that("mdi_disclosure_config coerces everything except MDIdomVar to double", {
  dir <- make_disclosure_fixture()
  disc <- mdi_disclosure_config(dir, "FI")

  expect_type(disc$MDIminNumObs, "double")
  expect_type(disc$MDIdomSh, "double")
  expect_type(disc$MDIdomVar, "character")
})

test_that("mdi_disclosure_config keeps MDIdomDirection as text", {
  dir <- paste0(tempfile("mdmeta_"), "/")
  dir.create(dir, recursive = TRUE)
  writeLines(c("disclosure_variable,FI,DE", "MDIminNumObs,3,", "MDIdomDirection,high,low"),
             paste0(dir, "MD_disclosure_info.csv"))
  expect_identical(mdi_disclosure_config(dir, "DE")$MDIdomDirection, "low")
  expect_identical(mdi_disclosure_config(dir, "FI")$MDIdomDirection, "high")
  # DE fills its numeric thresholds in itself; its direction is known
  expect_true(is.na(mdi_disclosure_config(dir, "DE")$MDIminNumObs))
})

test_that("mdi_disclosure_config names the countries when one is missing", {
  dir <- paste0(tempfile("disc_"), "/"); dir.create(dir)
  writeLines(c("disclosure_variable,FI,AT", "MDIminNumObs,3,10"), paste0(dir, "MD_disclosure_info.csv"))
  expect_error(mdi_disclosure_config(dir, "XX"), "no column for XX. Countries in it: FI, AT")
})
