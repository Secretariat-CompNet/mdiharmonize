make_coerce_vars <- function(name, class, domain = NA_character_) {
  data.table::data.table(MD_varname = name, class = class, domain = domain)
}

test_that("mdi_coerce_md_types converts to the declared class and logs NAs introduced", {
  dt <- data.table::data.table(emp = c("1", "x", NA), id = 1:3)
  res <- suppressWarnings(mdi_coerce_md_types(dt, "SBS", make_coerce_vars(c("emp", "id"), c("numeric", "character"))))
  expect_equal(res$data$emp, c(1, NA, NA))
  expect_equal(res$data$id, c("1", "2", "3"))
  emp <- res$log[res$log$MD_varname == "emp"]
  expect_equal(c(emp$na_before, emp$na_after, emp$na_introduced), c(1L, 2L, 1L))
  expect_equal(res$log$MD_dataset, c("SBS", "SBS"))
})

test_that("mdi_coerce_md_types counts numbers other than 0/1 converted to logical", {
  dt <- data.table::data.table(exporter = c(0, 1, 2, NA))
  res <- mdi_coerce_md_types(dt, "SBS", make_coerce_vars("exporter", "logical"))
  expect_equal(res$data$exporter, c(FALSE, TRUE, TRUE, NA))
  expect_equal(res$log$logical_out_of_domain, 1L)
  expect_equal(res$log$na_introduced, 0L)
})

test_that("mdi_coerce_md_types reads text logicals and dates with their format", {
  dt <- data.table::data.table(flag = c("T", "false", "maybe"), birth = c("01012000", "31122010", NA))
  res <- mdi_coerce_md_types(dt, "BR", make_coerce_vars(c("flag", "birth"), c("logical", "date"),
                                                       c(NA, "%d%m%Y")))
  expect_equal(res$data$flag, c(TRUE, FALSE, NA))
  expect_equal(res$data$birth, as.Date(c("2000-01-01", "2010-12-31", NA)))
})

test_that("mdi_coerce_md_types only makes factors of codebook-validated variables", {
  dt <- data.table::data.table(lfo = c("a", "b"), sector = c("x", "y"))
  vars <- make_coerce_vars(c("lfo", "sector"), c("factor", "factor"), c("MD_codebook", "MD_nace_class"))
  res <- mdi_coerce_md_types(dt, "BR", vars)
  expect_true(is.factor(res$data$lfo))
  expect_true(is.character(res$data$sector))
  expect_equal(res$log$MD_varname, "lfo")
})

test_that("mdi_coerce_md_types skips variables already right, missing, or without a class", {
  dt <- data.table::data.table(emp = c(1, 2))
  vars <- make_coerce_vars(c("emp", "absent", "emp"), c("numeric", "numeric", NA))
  res <- mdi_coerce_md_types(dt, "SBS", vars)
  expect_equal(nrow(res$log), 0L)
  expect_equal(names(res$log), c("MD_dataset", "MD_varname", "desired_class", "na_before",
                                 "na_after", "na_introduced", "logical_out_of_domain"))
})

test_that("mdi_coerce_md_types warns about a class it can't convert to", {
  dt <- data.table::data.table(emp = c(1, 2))
  expect_warning(res <- mdi_coerce_md_types(dt, "SBS", make_coerce_vars("emp", "complex")),
                 "complex")
  expect_equal(nrow(res$log), 0L)
})
