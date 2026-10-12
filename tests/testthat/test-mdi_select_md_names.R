sel <- function(vars = c("sales", "emp")) {
  data.table::data.table(MD_dataset = "SBS", MD_varname = vars)
}

make_select_cfg <- function(filtered = TRUE) {
  list(
    country = "FI",
    dirs = list(progs = tempdir()),
    flags = list(filtered_harmonization = filtered),
    modules = "ABCD",
    md = list(MD_varnames = data.table::data.table(
      MD_dataset = c("SBS", "SBS", "SBS", "SBS", "BR"),
      MD_varname = c("firmid", "year", "sales", "emp", "nace"),
      is_key     = c(1L, 1L, 0L, 0L, 0L)
    )),
    nsi = list(
      NSI_MD_conc = data.table::data.table(
        MD_dataset  = c("SBS", "SBS", "SBS", "SBS", "SBS", "BR"),
        MD_varname  = c("firmid", "year", "sales", "emp", "costs", "nace"),
        NSI_dataset = "sbs",
        NSI_varname = c("id", "jahr", "umsatz", "besch", "kosten", "wz")
      ),
      NSI_datafiles = data.table::data.table(
        NSI_datafile  = c("sbs2020", "br2020"),
        preprocessing = c("dedup|id#jahr|max|umsatz||filter|land|eq|FI", NA)
      ),
      NSI_varnames = data.table::data.table(
        NSI_datafile = c("sbs2020", "sbs2020", "sbs2020", "sbs2020"),
        NSI_varname  = c("id", "jahr", "umsatz", "land")
      )
    )
  )
}

test_that("filtered harmonization keeps the selected variables plus their datasets' keys", {
  out <- mdi_select_md_names(make_select_cfg(filtered = TRUE), sel())
  expect_setequal(out$nsi$NSI_MD_conc$MD_varname, c("firmid", "year", "sales", "emp"))
})

test_that("without filtered harmonization the concordance is left as is", {
  cfg <- make_select_cfg(filtered = FALSE)
  out <- mdi_select_md_names(cfg)
  expect_identical(out$nsi$NSI_MD_conc, cfg$nsi$NSI_MD_conc)
})

test_that("selected variables missing from MD_varnames are reported and ignored", {
  cfg <- make_select_cfg(filtered = TRUE)
  expect_message(out <- mdi_select_md_names(cfg, sel(c("sales", "not_a_var"))), "not in MD_varnames")
  expect_false("not_a_var" %in% out$nsi$NSI_MD_conc$MD_varname)
  expect_true("sales" %in% out$nsi$NSI_MD_conc$MD_varname)
})

test_that("prepro_vars lists the raw variables each file's preprocessing needs", {
  out <- mdi_select_md_names(make_select_cfg(filtered = FALSE))
  pv <- out$nsi$NSI_datafiles$prepro_vars
  # dedup keys + its max reference column + the filter column
  expect_setequal(pv[[1]], c("id", "jahr", "umsatz", "land"))
  expect_equal(pv[[2]], character(0))
})

test_that("mdi_select_md_names does not modify the tables in its input", {
  cfg <- make_select_cfg(filtered = TRUE)
  before <- data.table::copy(cfg$nsi)
  mdi_select_md_names(cfg, sel())
  expect_identical(cfg$nsi$NSI_MD_conc, before$NSI_MD_conc)
  expect_false("prepro_vars" %in% names(cfg$nsi$NSI_datafiles))
})

test_that("mdi_select_md_names needs the MD and NSI metadata loaded", {
  cfg <- make_select_cfg()
  cfg$nsi <- NULL
  expect_error(mdi_select_md_names(cfg), "NSI metadata loaded")
})

test_that("filtered harmonization needs the modules' selection", {
  expect_error(mdi_select_md_names(make_select_cfg(filtered = TRUE)), "'selected'")
})

test_that("mdi_load_module_selection stacks every module's MDnames_select file", {
  progs <- paste0(tempfile("progs_"), "/")
  for (m in c("ABCD", "EFGH")) {
    dir.create(paste0(progs, m), recursive = TRUE)
    data.table::fwrite(data.table::data.table(MD_dataset = "SBS", MD_varname = paste0("v_", m), note = "x"),
                       paste0(progs, m, "/", m, "_MDnames_select.csv"))
  }
  out <- mdi_load_module_selection(progs, c("ABCD", "EFGH"))
  expect_equal(names(out), c("MD_dataset", "MD_varname"))
  expect_equal(out$MD_varname, c("v_ABCD", "v_EFGH"))
  expect_error(mdi_load_module_selection(progs, "ZZZZ"), "ZZZZ")
})
