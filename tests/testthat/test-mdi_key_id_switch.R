make_md_varnames_fixture <- function() {
  data.table::data.table(
    MD_dataset = c("TEST", "TEST", "TEST"),
    MD_varname = c("plantid", "firmid", "sales"),
    is_key = c(1, 1, 0),
    aggregation_method = c(NA, NA, "sum"),
    disaggregation_method = c(NA, NA, "replicate")
  )
}

test_that("mdi_key_id_switch takes md_varnames directly, no global needed", {
  expect_false(exists("MD_varnames", inherits = FALSE))

  DT <- data.table::data.table(
    plantid = c("P1", "P2", "P3"),
    year = c(2020, 2020, 2020),
    sales = c(10, 20, 5)
  )
  conctab <- data.table::data.table(
    plantid = c("P1", "P2", "P3"),
    firmid = c("F1", "F1", "F2"),
    year = c(2020, 2020, 2020)
  )

  result <- mdi_key_id_switch(
    DT = DT, conctab = conctab,
    md_varnames = make_md_varnames_fixture(),
    method = "agg", obj_id_type = "to_firmid", key_id = "plantid"
  )

  expect_true("firmid" %in% names(result))
  expect_setequal(result$firmid, c("F1", "F2"))
  expect_equal(result[firmid == "F1"]$sales, 30)
  expect_equal(result[firmid == "F2"]$sales, 5)
})

test_that("mdi_key_id_switch disaggregates using the passed-in md_varnames rules", {
  DT <- data.table::data.table(
    firmid = c("F1", "F2"),
    year = c(2020, 2020),
    sales = c(30, 5)
  )
  conctab <- data.table::data.table(
    plantid = c("P1", "P2", "P3"),
    firmid = c("F1", "F1", "F2"),
    year = c(2020, 2020, 2020)
  )

  result <- mdi_key_id_switch(
    DT = DT, conctab = conctab,
    md_varnames = make_md_varnames_fixture(),
    method = "disagg", obj_id_type = "to_plantid", key_id = "firmid"
  )

  expect_true("plantid" %in% names(result))
  expect_setequal(result$plantid, c("P1", "P2", "P3"))
})

test_that("mdi_key_id_switch validates md_varnames is a data.table", {
  DT <- data.table::data.table(plantid = "P1", year = 2020, sales = 10)
  conctab <- data.table::data.table(plantid = "P1", firmid = "F1", year = 2020)
  expect_error(
    mdi_key_id_switch(DT, conctab, md_varnames = list(),
                       method = "agg", obj_id_type = "to_firmid", key_id = "plantid"),
    "data.table"
  )
})

test_that("mdi_key_id_switch rejects md_varnames spanning multiple datasets", {
  DT <- data.table::data.table(plantid = "P1", year = 2020, sales = 10)
  conctab <- data.table::data.table(plantid = "P1", firmid = "F1", year = 2020)
  unfiltered <- data.table::rbindlist(list(
    make_md_varnames_fixture(),
    data.table::copy(make_md_varnames_fixture())[, MD_dataset := "OTHER"]
  ))
  expect_error(
    mdi_key_id_switch(DT, conctab, md_varnames = unfiltered,
                       method = "agg", obj_id_type = "to_firmid", key_id = "plantid"),
    "single dataset"
  )
})

make_weighted_varnames_fixture <- function(agg_rule) {
  data.table::data.table(
    MD_dataset = c("TEST", "TEST", "TEST"),
    MD_varname = c("plantid", "firmid", "sales"),
    is_key = c(1, 1, 0),
    aggregation_method = c(NA, NA, agg_rule),
    disaggregation_method = c(NA, NA, agg_rule)
  )
}

test_that("mdi_key_id_switch warns once when a weighted_avg rule's weight column is absent", {
  DT <- data.table::data.table(
    plantid = c("P1", "P2"),
    year = c(2020, 2020),
    sales = c(10, 20)
  )
  conctab <- data.table::data.table(
    plantid = c("P1", "P2"),
    firmid = c("F1", "F1"),
    year = c(2020, 2020)
  )

  expect_warning(
    result <- mdi_key_id_switch(
      DT = DT, conctab = conctab,
      md_varnames = make_weighted_varnames_fixture("weighted_avg:wcol"),
      method = "agg", obj_id_type = "to_firmid", key_id = "plantid"
    ),
    "none of their weight column"
  )
  expect_equal(result$sales, 15)
})

test_that("mdi_key_id_switch does not warn when the weighted_avg weight column is present", {
  DT <- data.table::data.table(
    plantid = c("P1", "P2"),
    year = c(2020, 2020),
    sales = c(10, 20),
    wcol = c(1, 3)
  )
  conctab <- data.table::data.table(
    plantid = c("P1", "P2"),
    firmid = c("F1", "F1"),
    year = c(2020, 2020)
  )
  varnames <- data.table::rbindlist(list(
    make_weighted_varnames_fixture("weighted_avg:wcol"),
    data.table::data.table(MD_dataset = "TEST", MD_varname = "wcol", is_key = 0,
                            aggregation_method = "sum", disaggregation_method = "replicate")
  ))

  expect_warning(
    result <- mdi_key_id_switch(
      DT = DT, conctab = conctab, md_varnames = varnames,
      method = "agg", obj_id_type = "to_firmid", key_id = "plantid"
    ),
    NA
  )
  expect_equal(result$sales, weighted.mean(c(10, 20), c(1, 3)))
})

test_that("weighted_alloc without weight_data splits equally, with one warning", {
  DT <- data.table::data.table(firmid = "F1", year = 2020, sales = 10)
  conctab <- data.table::data.table(
    plantid = c("P1", "P2"),
    firmid = c("F1", "F1"),
    year = c(2020, 2020)
  )

  expect_warning(
    result <- suppressMessages(mdi_key_id_switch(
      DT = DT, conctab = conctab,
      md_varnames = make_weighted_varnames_fixture("weighted_alloc:BR.persons_br|equal"),
      method = "disagg", obj_id_type = "to_plantid", key_id = "firmid"
    )),
    "BR.persons_br \\(BR not in weight_data\\)"
  )
  expect_equal(sort(result$sales), c(5, 5))
})

make_alloc_case <- function() {
  list(
    DT = data.table::data.table(firmid = c("F1", "F2", "F3"), year = 2020, sales = c(100, 60, 30)),
    conctab = data.table::data.table(plantid = c("P1", "P2", "P3", "P4", "P5", "P6"),
                                     firmid = c("F1", "F1", "F2", "F2", "F3", "F3"), year = 2020),
    # F1's plants have BR employment; F2's only PRODCOM revenue, over two
    # products for P3; F3's neither
    br = data.table::data.table(plantid = c("P1", "P2", "P3", "P4"), year = 2020,
                                persons_br = c(30, 10, 0, NA)),
    prodcom = data.table::data.table(plantid = c("P3", "P3", "P4"), year = 2020, rev = c(10, 10, 20))
  )
}

test_that("weighted_alloc follows the chain per source unit, with weights from weight_data", {
  x <- make_alloc_case()
  md <- make_weighted_varnames_fixture("weighted_alloc:BR.persons_br|PRODCOM.rev|equal")
  expect_message(
    result <- mdi_key_id_switch(DT = x$DT, conctab = x$conctab, md_varnames = md,
                                method = "disagg", obj_id_type = "to_plantid", key_id = "firmid",
                                weight_data = list(BR = x$br, PRODCOM = x$prodcom)),
    "sales split by BR.persons_br for 1, PRODCOM.rev for 1, equal for 1 source unit"
  )
  data.table::setkeyv(result, "plantid")
  expect_equal(result$sales, c(75, 25, 30, 30, 15, 15))
  # the weights don't come out with the data
  expect_false(any(startsWith(names(result), ".w.")))
  expect_false(any(c("persons_br", "rev") %in% names(result)))
})

test_that("weighted_alloc never weights by the data's own column of the same name", {
  x <- make_alloc_case()
  DT <- x$DT[, persons_br := c(40, 40, 40)]
  md <- data.table::data.table(
    MD_dataset = "TEST", MD_varname = c("plantid", "firmid", "sales", "persons_br"),
    is_key = c(1, 1, 0, 0), aggregation_method = NA,
    disaggregation_method = c(NA, NA, "weighted_alloc:BR.persons_br|equal", "replicate"))
  result <- suppressMessages(mdi_key_id_switch(
    DT = DT, conctab = x$conctab, md_varnames = md, method = "disagg",
    obj_id_type = "to_plantid", key_id = "firmid", weight_data = list(BR = x$br)))
  data.table::setkeyv(result, "plantid")
  expect_equal(result$sales[1:2], c(75, 25))
  # the data's own persons_br is replicated as its rule says
  expect_equal(result$persons_br, rep(40, 6))
})

test_that("weighted_alloc skips a dataset at another level, and names it", {
  x <- make_alloc_case()
  br_firm <- data.table::data.table(firmid = "F1", year = 2020, persons_br = 40)
  md <- make_weighted_varnames_fixture("weighted_alloc:BR.persons_br|PRODCOM.rev|equal")
  expect_warning(
    result <- suppressMessages(mdi_key_id_switch(
      DT = x$DT, conctab = x$conctab, md_varnames = md, method = "disagg",
      obj_id_type = "to_plantid", key_id = "firmid",
      weight_data = list(BR = br_firm, PRODCOM = x$prodcom))),
    "BR.persons_br \\(BR not at plantid level\\)"
  )
  data.table::setkeyv(result, "plantid")
  expect_equal(result$sales, c(50, 50, 30, 30, 15, 15))
})

test_that("mdi_key_id_switch leaves the caller's concordance and weight tables as they were", {
  x <- make_alloc_case()
  # the id columns are converted to text inside; the caller's factor stays a factor
  x$conctab[, plantid := as.factor(plantid)]
  conc_before <- data.table::copy(x$conctab); br_before <- data.table::copy(x$br)
  suppressMessages(mdi_key_id_switch(
    DT = x$DT, conctab = x$conctab,
    md_varnames = make_weighted_varnames_fixture("weighted_alloc:BR.persons_br|equal"),
    method = "disagg", obj_id_type = "to_plantid", key_id = "firmid", weight_data = list(BR = x$br)))
  expect_identical(x$conctab, conc_before)
  expect_identical(x$br, br_before)
})

test_that("weight_data must be a named list", {
  x <- make_alloc_case()
  expect_error(mdi_key_id_switch(DT = x$DT, conctab = x$conctab, md_varnames = make_md_varnames_fixture(),
                                 method = "disagg", obj_id_type = "to_plantid", weight_data = x$br),
               "named list of data.tables")
})

test_that("a text column keeps its weighted_mode rule; a numeric rule on text falls back to mode with a warning", {
  DT <- data.table::data.table(
    plantid = c("P1", "P2", "P3"), year = 2020,
    nace = c("2511", "4671", "4671"), lfo = c("a", "b", "b"), persons_br = c(500, 5, 5)
  )
  conctab <- data.table::data.table(plantid = c("P1", "P2", "P3"), firmid = "F1", year = 2020)
  md <- data.table::data.table(
    MD_dataset = "TEST", MD_varname = c("plantid", "firmid", "nace", "lfo", "persons_br"),
    is_key = c(1, 1, 0, 0, 0),
    aggregation_method = c(NA, NA, "weighted_mode:persons_br|mode", "sum", "sum"),
    disaggregation_method = "replicate"
  )
  expect_warning(
    result <- mdi_key_id_switch(DT = DT, conctab = conctab, md_varnames = md,
                                method = "agg", obj_id_type = "to_firmid", key_id = "plantid"),
    "Aggregation rule 'sum' is not valid for text column 'lfo'"
  )
  # the plant with 500 employees decides, not the most frequent code
  expect_equal(result$nace, "2511")
  expect_equal(result$lfo, "b")
})

test_that("yes/no columns aggregate as logical, without coercion warnings", {
  DT <- data.table::data.table(plantid = c("P1", "P2", "P3"), year = 2020,
                               exporter = c(TRUE, FALSE, NA), rd = c(FALSE, FALSE, TRUE))
  conctab <- data.table::data.table(plantid = c("P1", "P2", "P3"), firmid = c("F1", "F1", "F2"), year = 2020)
  md <- data.table::data.table(MD_dataset = "TEST", MD_varname = c("plantid", "firmid", "exporter", "rd"),
                               is_key = c(1, 1, 0, 0), aggregation_method = c(NA, NA, "any", "all"),
                               disaggregation_method = "replicate")
  expect_no_warning(result <- mdi_key_id_switch(DT = DT, conctab = conctab, md_varnames = md,
                                                method = "agg", obj_id_type = "to_firmid", key_id = "plantid"))
  data.table::setorder(result, firmid)
  # it used to return the text "TRUE"/"FALSE"
  expect_type(result$exporter, "logical")
  # F2's only plant has exporter NA: stays NA, not FALSE
  expect_equal(result$exporter, c(TRUE, NA))
  expect_equal(result$rd, c(FALSE, TRUE))
})

test_that("weighted rules: ties go to the first code, zero or missing weights fall back", {
  DT <- data.table::data.table(
    plantid = c("P1", "P2", "P3", "P4", "P5", "P6"), year = 2020,
    nace = c("4671", "2511", "1011", "1012", "0111", "0112"),
    tfp = c(1, 3, 2, 4, 5, 7), persons_br = c(10, 10, 0, 0, 5, NA)
  )
  conctab <- data.table::data.table(plantid = DT$plantid, firmid = c("F1", "F1", "F2", "F2", "F3", "F3"), year = 2020)
  md <- data.table::data.table(
    MD_dataset = "TEST", MD_varname = c("plantid", "firmid", "nace", "tfp", "persons_br"), is_key = c(1, 1, 0, 0, 0),
    aggregation_method = c(NA, NA, "weighted_mode:persons_br|mode", "weighted_avg:persons_br", "sum"),
    disaggregation_method = "replicate"
  )
  result <- mdi_key_id_switch(DT = DT, conctab = conctab, md_varnames = md,
                              method = "agg", obj_id_type = "to_firmid", key_id = "plantid")
  data.table::setorder(result, firmid)
  # F1: tied at 10 employees each -> the first to appear (alphabetical would give 2511)
  expect_equal(result$nace[1], "4671")
  # F2: weights all 0 -> plain mean (weighted.mean gave NaN)
  expect_equal(result$tfp[2], 3)
  # F3: one weight missing -> the row with a weight (weighted.mean gave NA)
  expect_equal(result$tfp[3], 5)
})

test_that("survey weights of any dataset are dropped before the switch, with a warning", {
  DT <- data.table::data.table(plantid = c("P1", "P2"), year = 2020, sales = c(1, 2),
                               weights_sbs_nsi = 1.5, weights_bd_nsi = 2)
  conctab <- data.table::data.table(plantid = c("P1", "P2"), firmid = "F1", year = 2020)
  expect_warning(
    res <- mdi_key_id_switch(DT = DT, conctab = conctab, md_varnames = make_md_varnames_fixture(),
                             method = "agg", obj_id_type = "to_firmid", key_id = "plantid"),
    "weights_sbs_nsi, weights_bd_nsi")
  expect_false(any(grepl("^weights_", names(res))))
  expect_equal(res$sales, 3)
})

test_that("aggregating a group with only NAs gives NA, whatever the rule", {
  md <- data.table::data.table(
    MD_dataset = "TEST",
    MD_varname = c("plantid", "firmid", "sales", "exporter", "certified", "avg_wage"),
    is_key = c(1, 1, 0, 0, 0, 0),
    aggregation_method = c(NA, NA, "sum", "any", "all", "weighted_avg:sales"),
    disaggregation_method = NA
  )
  DT <- data.table::data.table(
    plantid = c("P1", "P2", "P3"), year = 2020,
    sales = c(NA, NA, 5), exporter = c(NA, NA, TRUE), certified = c(NA, NA, FALSE),
    avg_wage = c(NA, NA, 3)
  )
  conctab <- data.table::data.table(plantid = c("P1", "P2", "P3"), firmid = c("F1", "F1", "F2"), year = 2020)
  res <- mdi_key_id_switch(DT, conctab, md, method = "agg", obj_id_type = "to_firmid", key_id = "plantid")
  f1 <- res[firmid == "F1"]
  expect_true(is.na(f1$sales))
  expect_true(is.na(f1$exporter))
  expect_true(is.na(f1$certified))
  expect_true(is.na(f1$avg_wage) && !is.nan(f1$avg_wage))
  expect_true(is.logical(res$exporter))
  expect_equal(res[firmid == "F2"]$sales, 5)
})

test_that("rows the concordance can't assign are dropped, with a count per year", {
  DT <- data.table::data.table(plantid = c("P1", "P2", "P9"), year = c(2020, 2020, 2021), sales = c(10, 20, 5))
  conctab <- data.table::data.table(plantid = c("P1", "P2"), firmid = c("F1", "F1"), year = 2020)
  expect_message(
    res <- mdi_key_id_switch(DT, conctab, make_md_varnames_fixture(), method = "agg",
                             obj_id_type = "to_firmid", key_id = "plantid"),
    "1 row\\(s\\) with a plantid .* 2021: 1"
  )
  expect_false(anyNA(res$firmid))
  expect_equal(res$sales, 30)

  firm <- data.table::data.table(firmid = c("F1", "F9"), year = 2020, sales = c(10, 7))
  conc2 <- data.table::data.table(firmid = c("F1", "F1"), plantid = c("P1", "P2"), year = 2020)
  expect_message(
    res2 <- mdi_key_id_switch(firm, conc2, make_md_varnames_fixture(), method = "disagg",
                              obj_id_type = "to_plantid", key_id = "firmid"),
    "1 row\\(s\\) with a firmid"
  )
  expect_false(anyNA(res2$plantid))
  expect_setequal(res2$plantid, c("P1", "P2"))
})
