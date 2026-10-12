test_that("mdi_check_safe_expr allows whitelisted syntax and functions", {
  expect_true(mdi_check_safe_expr("x * 3.14159"))
  expect_true(mdi_check_safe_expr("log(x)"))
  expect_true(mdi_check_safe_expr("as.numeric(x) / 1000"))
  expect_true(mdi_check_safe_expr("ifelse(is.na(x), NA, 1)"))
  expect_true(mdi_check_safe_expr("format(as.Date(x), '%Y')"))
  expect_true(mdi_check_safe_expr("case_when(x == 1 ~ 'a', TRUE ~ 'b')"))
  expect_true(mdi_check_safe_expr("as.logical(recode(x, '1' = 'TRUE'))"))
  expect_true(mdi_check_safe_expr("as.character(str_remove_all(x, '\\\\.'))"))
  # used in older metadata
  expect_true(mdi_check_safe_expr("sub('a', 'b', x)"))
  expect_true(mdi_check_safe_expr("substring(x, 1, 3)"))
  expect_true(mdi_check_safe_expr("sum(x)"))
  expect_true(mdi_check_safe_expr("trimws(x)"))
  # common safe buffer, not yet seen in real data
  expect_true(mdi_check_safe_expr("sqrt(abs(x))"))
  expect_true(mdi_check_safe_expr("toupper(trimws(x))"))
})

test_that("mdi_check_safe_expr's rejection error names the function and explains how to fix it", {
  err <- tryCatch(mdi_check_safe_expr("system('id')"), error = function(e) e)
  expect_match(conditionMessage(err), "'system' is not on the allowed function list", fixed = TRUE)
  expect_match(conditionMessage(err), "add it to .mdi_safe_fns in mdi_check_safe_expr.R", fixed = TRUE)
})

test_that("mdi_check_safe_expr rejects functions outside the whitelist", {
  expect_error(mdi_check_safe_expr("system('rm -rf /')"), "system")
  expect_error(mdi_check_safe_expr("file.remove('x')"), "file.remove")
  expect_error(mdi_check_safe_expr("Sys.setenv(x = 1)"), "Sys.setenv")
  expect_error(mdi_check_safe_expr("source('x.R')"), "source")
  expect_error(mdi_check_safe_expr("unlink('x')"), "unlink")
})

test_that("mdi_check_safe_expr rejects a disallowed function nested inside allowed ones", {
  expect_error(mdi_check_safe_expr("ifelse(is.na(x), system('id'), 1)"), "system")
})

test_that("mdi_check_safe_expr errors clearly on unparseable text", {
  expect_error(mdi_check_safe_expr("x +* 1"), "could not parse")
})

test_that("mdi_check_safe_expr never restricts bare symbols (column/variable names)", {
  expect_true(mdi_check_safe_expr("colA == 2 & colB == 'X'"))
  expect_true(mdi_check_safe_expr("DT[, x := 1]"))
})

test_that("real revalue-shaped detail strings pass validation and match bare eval() results", {
  # Exact shape of mdi_harmonize()'s revalue block: x substituted for the
  # column name, then validate-and-eval.
  real_details <- c(
    "x * 3.14159", "as.numeric(x) / 1000", "log(x)",
    "as.character(as.integer(x))", "as.logical(x)",
    "ifelse(is.na(x), NA, 1)", "substr(x, 1, 3)",
    "format(as.Date(x), '%Y')"
  )
  for (compute_str in real_details) {
    DT <- data.table::data.table(mycol = c("2020", "5", "10"))
    expr_str <- gsub("\\bx\\b", "mycol", compute_str, perl = TRUE)
    mdi_check_safe_expr(expr_str)
    result_new <- tryCatch(DT[, out := eval(parse(text = expr_str))]$out,
                            error = function(e) e)
    DT2 <- data.table::data.table(mycol = c("2020", "5", "10"))
    result_old <- tryCatch(DT2[, out := eval(parse(text = expr_str))]$out,
                            error = function(e) e)
    expect_equal(result_new, result_old)
  }
})

test_that("real redefine-shaped '+'/'-' expressions pass validation and evaluate correctly", {
  DT <- data.table::data.table(v1 = c(1, NA, 3), v2 = c(10, 20, NA))
  tmp_var_prefix <- "tmp_"
  varright <- "total"
  expr <- paste0(
    "DT[, paste0(tmp_var_prefix, '", varright, "') := {",
    "tmp_vars <- lapply(.SD, function(x) fifelse(is.na(as.numeric(x)), 0, as.numeric(x)));",
    "res <- Reduce(`+`, tmp_vars);",
    "all_na <- Reduce(`&`, lapply(.SD, is.na));",
    "fifelse(all_na, NA_real_, res)",
    "}, .SDcols = c('v1', 'v2')]"
  )
  mdi_check_safe_expr(expr)
  eval(parse(text = expr))
  expect_equal(DT$tmp_total, c(11, 20, 3))
})

test_that("derive-shaped condition strings pass validation and filter correctly", {
  conditions <- c("is.na(yr)", "!is.na(sales_a)&is.na(sales_b)", "status==1", "code == 2")
  for (cond in conditions) {
    expect_true(mdi_check_safe_expr(cond))
  }
  dt <- data.table::data.table(status = c(1, 2, 1), flag = NA_character_)
  condition <- "status==1"
  mdi_check_safe_expr(condition)
  dt[eval(parse(text = condition)), flag := "matched"]
  expect_equal(dt$flag, c("matched", NA, "matched"))
})

test_that("a function handed to lapply/mapply/Reduce must be allowed too", {
  expect_error(mdi_check_safe_expr("lapply(x, system)"), "'system' is not on the allowed")
  expect_error(mdi_check_safe_expr("Reduce('system', x)"), "'system' is not on the allowed")
  expect_error(mdi_check_safe_expr("mapply(FUN = get, x)"), "'get' is not on the allowed")
  expect_silent(mdi_check_safe_expr("Reduce(`+`, lapply(.SD, is.na))"))
  expect_silent(mdi_check_safe_expr("lapply(.SD, function(x) fifelse(is.na(x), 0, x))"))
  expect_error(mdi_check_safe_expr("lapply(.SD, function(x) system(x))"), "'system'")
})
