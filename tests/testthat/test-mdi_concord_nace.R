make_break_fixture <- function(weight_col_name = "wt") {
  # Two firms, both switching from nace "A1" to "B2" at year 2020 -- a clean,
  # unambiguous break year with a 1:1 concordance.
  dt <- data.table::data.table(
    fid = rep(c("F1", "F2"), each = 5),
    year = rep(2018:2022, times = 2),
    nace = rep(c("A1", "A1", "B2", "B2", "B2"), times = 2)
  )
  dt[[weight_col_name]] <- 10
  dt
}

test_that("mdi_concord_nace takes DT and id_col directly, no globals needed", {
  dt <- make_break_fixture()
  expect_false(exists("dirTMPSAVE", inherits = FALSE))
  expect_false(exists("MD_idInfo", inherits = FALSE))
  result <- mdi_concord_nace(dt, "fid", weight_emp = FALSE, cumsh_del = 0.95, sh_del = 0.02)
  expect_true("MDnace" %in% names(result))
  expect_equal(nrow(result), nrow(dt))
})

test_that("mdi_concord_nace harmonizes the pre-break code to the post-break code", {
  dt <- make_break_fixture()
  result <- mdi_concord_nace(dt, "fid", weight_emp = FALSE, cumsh_del = 0.95, sh_del = 0.02)
  # Both firms consistently used "B2" post-break, so pre-break "A1" rows
  # should be harmonized forward to "B2" too.
  expect_true(all(result$MDnace == "B2"))
})

test_that("mdi_concord_nace's weight_col parameter is honored, not hardcoded to persons_br", {
  dt <- make_break_fixture(weight_col_name = "custom_weight")
  expect_false("persons_br" %in% names(dt))
  result <- mdi_concord_nace(dt, "fid", weight_emp = TRUE, cumsh_del = 0.95, sh_del = 0.02,
                              weight_col = "custom_weight")
  expect_true("MDnace" %in% names(result))
})

test_that("mdi_concord_nace works with a non-standard id_col name", {
  dt <- make_break_fixture()
  data.table::setnames(dt, "fid", "plant_identifier")
  result <- mdi_concord_nace(dt, "plant_identifier", weight_emp = FALSE, cumsh_del = 0.95, sh_del = 0.02)
  expect_true("plant_identifier" %in% names(result))
})

test_that("mdi_concord_nace errors clearly on a missing id_col", {
  dt <- make_break_fixture()
  expect_error(mdi_concord_nace(dt, NA_character_, FALSE, 0.95, 0.02), "Missing firm identifier")
  expect_error(mdi_concord_nace(dt, "", FALSE, 0.95, 0.02), "Missing firm identifier")
})

test_that("mdi_concord_nace does not mutate the caller's input data.table", {
  dt <- make_break_fixture()
  original_names <- names(dt)
  mdi_concord_nace(dt, "fid", weight_emp = FALSE, cumsh_del = 0.95, sh_del = 0.02)
  expect_equal(names(dt), original_names)
})

test_that(".fill_in_group fills from the same group only", {
  g <- c(1, 1, 1, 2, 2)
  x <- c(NA, "a", NA, NA, "b")
  expect_equal(.fill_in_group(x, g, "locf"), c(NA, "a", "a", NA, "b"))
  expect_equal(.fill_in_group(x, g, "nocb"), c("a", "a", NA, "b", "b"))
})

test_that(".lag_in_group is NA in each group's first row", {
  expect_equal(.lag_in_group(c("a", "b", "c", "d"), c(1, 1, 2, 2)), c(NA, "a", NA, "c"))
  expect_equal(.lag_in_group(1:3, c(1, 1, 1)), c(NA, 1L, 2L))
})

test_that("mdi_concord_nace names the columns it needs and doesn't find", {
  dt <- data.table::data.table(entid = "a", year = 2020, nace = "0111")
  expect_error(mdi_concord_nace(dt, "entid", TRUE, 0.95, 0.02), "column\\(s\\) persons_br not in the data")
  expect_error(mdi_concord_nace(dt[, -"nace"], "entid", FALSE, 0.95, 0.02), "column\\(s\\) nace not in the data")
})

test_that("mdi_concord_nace gives the same result on every run, and leaves the caller's random numbers alone", {
  set.seed(42)
  n <- 300
  br <- data.table::data.table(entid = rep(as.character(1:n), each = 4), year = rep(2005:2008, n))
  br[, nace := ifelse(year < 2007, sample(c("5211", "5212", "5213"), .N, TRUE), sample(c("4711", "4719"), .N, TRUE)), by = entid]
  br[, persons_br := 1]
  # firms that only exist before the break get their code by a random draw
  br <- br[!(entid %in% as.character(1:100) & year >= 2007)]
  set.seed(1); u1 <- suppressWarnings(mdi_concord_nace(data.table::copy(br), "entid", FALSE, 0.95, 0.02, seed = NULL))
  set.seed(2); u2 <- suppressWarnings(mdi_concord_nace(data.table::copy(br), "entid", FALSE, 0.95, 0.02, seed = NULL))
  expect_false(identical(u1$MDnace, u2$MDnace))
  set.seed(7); before <- runif(1); set.seed(7)
  a <- suppressWarnings(mdi_concord_nace(data.table::copy(br), "entid", FALSE, 0.95, 0.02))
  after <- runif(1)
  b <- suppressWarnings(mdi_concord_nace(data.table::copy(br), "entid", FALSE, 0.95, 0.02))
  expect_identical(a, b)
  # the caller's stream continues as if the function hadn't drawn anything
  expect_identical(after, before)
  # seed = NULL draws from the caller's stream
  set.seed(1); c1 <- suppressWarnings(mdi_concord_nace(data.table::copy(br), "entid", FALSE, 0.95, 0.02, seed = NULL))
  expect_identical(c1, a)
})
