test_that("dedup|na keeps the row with fewest NAs", {
  dt <- data.table::data.table(
    id = c(1, 1, 2),
    a  = c(NA, 5, 1),
    b  = c(NA, NA, 2)
  )
  out <- mdi_preprocess("dedup|id|na", dt)
  data.table::setorder(out, id)
  expect_equal(nrow(out), 2)
  expect_equal(out[id == 1]$a, 5)
})

test_that("dedup|max keeps the row with the max dedup_col value", {
  dt <- data.table::data.table(id = c(1, 1, 2), v = c(10, 20, 1))
  out <- mdi_preprocess("dedup|id|max|v", dt)
  data.table::setorder(out, id)
  expect_equal(out[id == 1]$v, 20)
})

test_that("dedup|meanmode aggregates numeric via mean and categorical via mode", {
  dt <- data.table::data.table(
    id = c(1, 1, 2, 2),
    n  = c(10, 20, 5, 5),
    c  = c("x", "x", "y", "z")
  )
  out <- mdi_preprocess("dedup|id|meanmode", dt)
  data.table::setorder(out, id)
  expect_equal(out[id == 1]$n, 15)
  expect_equal(out[id == 1]$c, "x")
})

test_that("dedup|random keeps exactly one row per group", {
  dt <- data.table::data.table(id = c(1, 1, 1, 2), v = 1:4)
  out <- mdi_preprocess("dedup|id|random", dt)
  expect_equal(nrow(out), 2)
})

test_that("filter supports eq/gt/in", {
  dt <- data.table::data.table(country = c("IT", "DE", "FR"), val = c(1, 5, 10))
  expect_equal(nrow(mdi_preprocess("filter|country|eq|IT", dt)), 1)
  expect_equal(nrow(mdi_preprocess("filter|val|gt|3", dt)), 2)
  expect_equal(nrow(mdi_preprocess("filter|country|in|IT#DE", dt)), 2)
})

test_that("filter rejects a malformed instruction instead of erroring on an undefined variable", {
  dt <- data.table::data.table(country = "IT")
  expect_error(
    mdi_preprocess("filter|country|eq", dt),
    "exactly 3 parts"
  )
})

test_that("agg sums/means and supports pickmaxby", {
  dt <- data.table::data.table(
    grp = c("a", "a", "b"),
    val = c(1, 2, 10),
    ref = c(1, 5, 1),
    tag = c("x", "y", "z")
  )
  out <- mdi_preprocess("agg|grp|val:sum#tag:pickmaxby-ref", dt)
  data.table::setorder(out, grp)
  expect_equal(out[grp == "a"]$val, 3)
  expect_equal(out[grp == "a"]$tag, "y")
})

test_that("restruct drops a column and dedups on the remaining ones", {
  dt <- data.table::data.table(id = c(1, 1, 2), drop_me = c("a", "b", "c"), keep = c(1, 1, 2))
  out <- mdi_preprocess("restruct|drop_me|id#keep", dt)
  expect_equal(nrow(out), 2)
  expect_false("drop_me" %in% names(out))
})

test_that("reshape casts long to wide", {
  dt <- data.table::data.table(id = c(1, 1), grp = c("a", "b"), val = c(10, 20))
  out <- mdi_preprocess("reshape|id|grp|val", dt)
  expect_equal(out[id == 1]$a, 10)
  expect_equal(out[id == 1]$b, 20)
})

test_that("derive|condmap applies conditions in order with a default fallback", {
  dt <- data.table::data.table(type = c(1, 2, 3), colA = c("A1", "A2", "A3"))
  # "none" isn't a column: written as text, with a warning
  expect_warning(out <- mdi_preprocess(
    "derive|derived|condmap|type == 1:colA#type == 2:colA|default:none", dt
  ), "none is not a column")
  expect_equal(out$derived, c("A1", "A2", "none"))
})

test_that("scaleif scales target columns conditionally", {
  dt <- data.table::data.table(unit_code = c(1, 2), value = c(10, 10))
  out <- mdi_preprocess("scaleif|unit_code|1:1#2:1000|value", dt)
  expect_equal(out$value, c(10, 10000))
})

test_that("trimchars trims the last n characters", {
  dt <- data.table::data.table(code = c("ABC12", "XYZ99"))
  out <- mdi_preprocess("trimchars|code|2", dt)
  expect_equal(out$code, c("ABC", "XYZ"))
})

test_that("parsedate parses a date column with an explicit informat", {
  dt <- data.table::data.table(d = c("2020-01-15", "2020-02-20"))
  out <- mdi_preprocess("parsedate|d|%Y-%m-%d", dt)
  expect_equal(out$d, as.Date(c("2020-01-15", "2020-02-20")))
})

test_that("mergefrom requires read_datafile and errors clearly without it", {
  dt <- data.table::data.table(id = 1, year = 2020)
  expect_error(
    mdi_preprocess("mergefrom|other|id#year|val|left", dt),
    "requires read_datafile"
  )
})

test_that("mergefrom reads the other file with read_datafile and left-joins it", {
  other <- data.table::data.table(id = c(1, 2), year = c(2020, 2020), val = c("x", "y"), extra = 0)
  asked <- NULL
  read_datafile <- function(datafile, cols, text_cols) {
    asked <<- list(datafile = datafile, cols = cols, text_cols = text_cols)
    other
  }
  dt <- data.table::data.table(id = c(1, 2, 3), year = c(2020, 2020, 2020))
  out <- mdi_preprocess("mergefrom|other|id#year|val|left", dt, read_datafile = read_datafile)
  data.table::setorder(out, id)
  expect_equal(out$val, c("x", "y", NA))
  expect_false("extra" %in% names(out))
  expect_equal(asked, list(datafile = "other", cols = c("id", "year", "val"), text_cols = character(0)))
})

test_that("mergefrom asks for text join keys as text and stops on mixed key types", {
  dt <- data.table::data.table(id = c("007", "010"), year = 2020)
  other <- data.table::data.table(id = c("007", "010"), year = 2020, val = 1:2)
  out <- mdi_preprocess("mergefrom|other|id#year|val", data.table::copy(dt),
                        read_datafile = function(datafile, cols, text_cols) {
                          expect_equal(text_cols, "id")
                          other
                        })
  expect_equal(out[order(id)]$val, 1:2)

  other_num <- data.table::data.table(id = c(7, 10), year = 2020, val = 1:2)
  expect_error(
    mdi_preprocess("mergefrom|other|id#year|val", data.table::copy(dt),
                   read_datafile = function(...) other_num),
    "join key\\(s\\) id are text on one side and numbers"
  )
})

test_that("mergefrom names the columns the other file lacks", {
  dt <- data.table::data.table(id = 1, year = 2020)
  expect_error(
    mdi_preprocess("mergefrom|other|id#year|val", dt,
                   read_datafile = function(...) data.table::data.table(id = 1, year = 2020)),
    "'other' lacks column\\(s\\): val"
  )
})

test_that("the pipeline reads a mergefrom file with its own row and declared classes", {
  tmp_dir <- paste0(tempfile(), "/")
  dir.create(paste0(tmp_dir, "sub"), recursive = TRUE)
  data.table::fwrite(data.table::data.table(id = c("007", "010"), val = c(1, 2), skip = 0),
                     paste0(tmp_dir, "sub/other.csv"))
  cfg <- list(
    country = "XX", dirs = list(inputdata = tmp_dir),
    nsi = list(
      NSI_datafiles = data.table::data.table(NSI_datafile = c("main", "other"), path = c("", "sub/"),
                                             format = "csv", encoding = c("latin1", "UTF-8")),
      NSI_varnames = data.table::data.table(NSI_datafile = "other", NSI_varname = c("id", "val"),
                                            class = c("character", "numeric"))
    )
  )
  out <- .read_merge_source(cfg, "other", c("id", "val"), character(0), NULL)
  expect_equal(out$id, c("007", "010"))
  expect_equal(names(out), c("id", "val"))
  expect_error(.read_merge_source(cfg, "nope", "id", character(0), NULL),
               "'nope' must be exactly one NSI_datafile of XX_datafiles.csv, found 0")
})

test_that("an unknown operation stops instead of being skipped", {
  dt <- data.table::data.table(id = 1)
  # a filter step that lost its "filter|" prefix
  expect_error(mdi_preprocess("SALES_CODE|nin|A#B", dt), "unknown operation 'SALES_CODE'")
  expect_error(mdi_preprocess("filter|id|eq|1||dedupe|id|na", dt), "unknown operation 'dedupe'")
})

test_that("malformed dedup and derive steps stop", {
  dt <- data.table::data.table(id = 1, v = 1)
  expect_error(mdi_preprocess("dedup|id|maximum|v", dt), "dedup method must be one of")
  expect_error(mdi_preprocess("dedup|id|max", dt), "needs the column to compare")
  expect_error(mdi_preprocess("derive|new|v == 1:a|default:b", dt), "derive needs the form")
})

test_that(".parse_preprocessing gives each step's arguments named after prep_<op>()", {
  steps <- .parse_preprocessing("dedup|id#year|max|emp||agg|id|v:sum#t:pickmaxby-v||parsedate|d|%d%m%Y|%Y")
  expect_equal(vapply(steps, `[[`, "", "op"), c("dedup", "agg", "parsedate"))
  expect_equal(steps[[1]]$args, list(unique_vars = c("id", "year"), method = "max", dedupvar = "emp"))
  expect_equal(steps[[2]]$args$custom_pickmax, list(t = "v"))
  expect_equal(steps[[3]]$args[c("lang", "outformat")], list(lang = "en", outformat = "%Y"))
  expect_identical(.parse_preprocessing(NA_character_), list())
})

test_that("dedup keeps the by-columns first and the groups in order of first appearance", {
  dt <- data.table::data.table(v = c(1, 9, 5, NA), id = c("b", "a", "b", "a"), w = c(NA, 2, 3, 4))
  out <- mdi_preprocess("dedup|id|na", dt)
  expect_equal(names(out), c("id", "v", "w"))
  expect_equal(out$id, c("b", "a"))
  expect_equal(out$v, c(5, 9))
})

test_that("agg's mode takes the most frequent value per group, the first to appear on a tie", {
  dt <- data.table::data.table(g = c(2, 2, 2, 2, 1, 1, 3), s = c("y", "x", "x", "y", NA, "z", NA))
  out <- mdi_preprocess("agg|g|s:mode", dt)
  expect_equal(out$g, c(2, 1, 3))
  expect_equal(out$s, c("y", "z", NA))
})

test_that("a step reading a column the data doesn't have stops and names it", {
  dt <- data.table::data.table(id = c(1, 1), v = c(1, 2), k = c("a", "b"))
  # dedup max used to silently keep the first row
  expect_error(mdi_preprocess("dedup|id|max|nope", dt), "'dedup\\|id\\|max\\|nope': column\\(s\\) nope not in the data")
  expect_error(mdi_preprocess("agg|id|nope:sum", dt), "column\\(s\\) nope not in the data")
  # checked against the data as the earlier steps left it
  expect_error(mdi_preprocess("agg|id|v:sum||filter|k|eq|a", dt), "'filter\\|k\\|eq\\|a': column\\(s\\) k")
  # a derive condition may only name columns (year would find data.table's year())
  expect_error(mdi_preprocess("derive|n|condmap|year == 2020:k|default:v", dt), "column\\(s\\) year not in the data")
})

test_that("a derive value that looks like a column but isn't one is warned about and written as text", {
  dt <- data.table::data.table(v = c(1, 2), k = c("a", "b"))
  expect_warning(out <- mdi_preprocess("derive|n|condmap|v == 2:JAHR|default:k", dt), "JAHR is not a column")
  expect_equal(out$n, c("a", "JAHR"))
  expect_silent(mdi_preprocess("derive|n|condmap|v == 2:k|default:NA", data.table::copy(dt)))
})

test_that("filter isna/notna keep rows by missingness and take no value", {
  dt <- data.table::data.table(id = 1:3, x = c(NA, "a", "b"))
  expect_equal(mdi_preprocess("filter|x|isna", data.table::copy(dt))$id, 1L)
  expect_equal(mdi_preprocess("filter|x|notna", data.table::copy(dt))$id, 2:3)
  expect_error(mdi_preprocess("filter|x|notna|1", dt), "takes no value")
})

test_that("reshapelong turns measure columns into rows", {
  dt <- data.table::data.table(id = 1:2, import = c(10, NA), export = c(5, 7))
  out <- mdi_preprocess("reshapelong|id|import#export|dir|val||filter|val|notna", dt)
  expect_equal(names(out), c("id", "dir", "val"))
  expect_equal(out$dir, c("import", "export", "export"))
  expect_type(out$dir, "character")
  expect_error(mdi_preprocess("reshapelong|id|import|dir", dt), "reshapelong needs the form")
})

test_that("agg skips missing values, and an all-missing group gives NA", {
  dt <- data.table::data.table(g = c(1, 1, 2, 2), x = c(1, NA, NA, NA), w = c(NA, NA, 1, 3), v = c(5, 6, 7, 8))
  out <- mdi_preprocess("agg|g|x:sum#v:pickmaxby-w", dt)
  # sum used to be NA for a group with a single NA
  expect_equal(out$x, c(1, NA))
  # no reference value in group 1: nothing to pick (the old code dropped the group)
  expect_equal(out$v, c(NA, 8))
})

test_that("agg only takes the supported functions", {
  dt <- data.table::data.table(g = c(1, 1), v = c(1, 2))
  expect_error(mdi_preprocess("agg|g|v:system", dt), "aggregation function 'system' isn't supported")
  expect_equal(mdi_preprocess("agg|g|v:sum", data.table::copy(dt))$v, 3)
})

test_that("random dedup keeps the same row on every run, and leaves the caller's random numbers alone", {
  dt <- data.table::data.table(id = rep(1:50, each = 3), v = seq_len(150))
  a <- mdi_preprocess("dedup|id|random", data.table::copy(dt))
  set.seed(7); expected <- runif(2)
  set.seed(7); b <- mdi_preprocess("dedup|id|random", data.table::copy(dt)); after <- runif(2)
  expect_identical(a, b)
  expect_equal(after, expected)
})
