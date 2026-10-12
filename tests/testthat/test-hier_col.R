make_hier_levels <- function() {
  data.table::data.table(hier = "MD_nuts_hier", level = c("h_0", "h_1", "h_2"),
                         level_name = c("lau", "nuts3", "nuts2"),
                         md_varname = c("lau", "nuts3", "nuts2"))
}

test_that("hier_col finds a level by name or by MD variable", {
  hl <- make_hier_levels()
  expect_equal(hier_col("MD_nuts_hier", level_name = "nuts2", hlevels = hl), "h_2")
  expect_equal(hier_col("MD_nuts_hier", md_varname = "nuts3", hlevels = hl), "h_1")
  expect_error(hier_col("MD_nuts_hier", hlevels = hl), "exactly one of level_name or md_varname")
})

test_that("hier_col needs hlevels, and says what to pass", {
  # even with an MD_hier_levels in sight: nothing is looked up implicitly
  MD_hier_levels <- make_hier_levels()
  expect_error(hier_col("MD_nuts_hier", level_name = "nuts2"), "'hlevels' is required.*hlevels = MD_hier_levels")
  expect_error(hier_col("MD_nuts_hier", level_name = "nuts2", hlevels = data.frame(x = 1)), "hlevels")
})
