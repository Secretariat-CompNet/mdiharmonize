# The steps around the branching are mocked: these tests are about which
# steps run, the clean-up and the Oracle connection.
make_pipeline_cfg <- function(import = TRUE, clean_tmp = FALSE, db_user = NULL) {
  tmp <- paste0(tempdir(), "/pipeline_tmp/")
  dir.create(tmp, showWarnings = FALSE)
  file.create(paste0(tmp, "OLD.RDS"))
  list(dirs = list(tmpsave = tmp), country = "FI", md = list(), db_user = db_user,
       flags = list(import = import, clean_tmp = clean_tmp, filtered_harmonization = FALSE))
}

mock_pipeline_steps <- function(env = parent.frame(), formats = "csv") {
  calls <- new.env()
  calls$steps <- character(0)
  step <- function(name) function(...) { calls$steps <- c(calls$steps, name); invisible(NULL) }
  testthat::local_mocked_bindings(
    mdi_load_nsi_metadata = function(...) list(NSI_MD_conc = data.table::data.table(),
                                              NSI_datafiles = data.table::data.table(format = formats)),
    mdi_select_md_names = function(cfg, selected) cfg,
    mdi_classify_conc = function(...) data.table::data.table(),
    mdi_conc_order = function(x) x,
    mdi_read_firmdata = function(cfg, conctype, oracle_channel) {
      calls$steps <- c(calls$steps, "read")
      calls$channel <- oracle_channel
    },
    mdi_harmonize_class_vars = step("class"),
    mdi_add_pim_capital = step("pim"),
    mdi_load_deflators = function(...) list(deflators = "d"),
    .oracle_connect = function(cfg) { calls$steps <- c(calls$steps, "connect"); "channel" },
    .oracle_close = function(channel) calls$steps <- c(calls$steps, "close"),
    .env = env
  )
  calls
}

test_that("mdi_harmonize_pipeline imports, harmonizes classes and adds PIM, in that order", {
  calls <- mock_pipeline_steps()
  capture.output(cfg <- mdi_harmonize_pipeline(make_pipeline_cfg()))
  expect_equal(calls$steps, c("read", "class", "pim"))
  expect_false(is.null(cfg$nsi))
  expect_false(is.null(cfg$conctype))
  expect_equal(cfg$deflators$deflators, "d")
})

test_that("mdi_harmonize_pipeline without an import only loads the metadata", {
  calls <- mock_pipeline_steps()
  capture.output(mdi_harmonize_pipeline(make_pipeline_cfg(import = FALSE)))
  expect_length(calls$steps, 0)
})

test_that("mdi_harmonize_pipeline cleans a previous run's datasets before importing", {
  mock_pipeline_steps()
  cfg <- make_pipeline_cfg(clean_tmp = TRUE)
  capture.output(mdi_harmonize_pipeline(cfg))
  expect_false(file.exists(file.path(cfg$dirs$tmpsave, "OLD.RDS")))
})

test_that("mdi_harmonize_pipeline keeps the datasets it reads when there's no import", {
  mock_pipeline_steps()
  cfg <- make_pipeline_cfg(import = FALSE, clean_tmp = TRUE)
  expect_warning(capture.output(mdi_harmonize_pipeline(cfg)), "not cleaned")
  expect_true(file.exists(file.path(cfg$dirs$tmpsave, "OLD.RDS")))
})

test_that("with MDIdatasets, the import gets only the chosen datasets' concordance", {
  conc <- data.table::data.table(NSI_dataset = c("reg", "reg", "svy"), MD_dataset = c("BR", "SBS", "ITGS"))
  cfg <- list(country = "FI", datasets = c("SBS", "ITGS"), flags = list(), nsi = list(NSI_MD_conc = conc))
  expect_equal(.only_chosen_datasets(cfg)$nsi$NSI_MD_conc$MD_dataset, c("SBS", "ITGS"))
  cfg$datasets <- NULL
  expect_identical(.only_chosen_datasets(cfg), cfg)
  cfg$datasets <- c("SBS", "PRODCOM")
  expect_error(.only_chosen_datasets(cfg), "PRODCOM not mapped for FI. The MD datasets it maps: BR, ITGS, SBS")
})

test_that("with MDIdatasets, the pipeline cleans only their files and redoes PIM only for SBS or BR", {
  calls <- mock_pipeline_steps()
  testthat::local_mocked_bindings(
    .only_chosen_datasets = function(cfg) cfg,
    mdi_classify_conc = function(...) { calls$steps <- c(calls$steps, "order"); data.table::data.table() }
  )
  cfg <- make_pipeline_cfg(clean_tmp = TRUE)
  file.create(file.path(cfg$dirs$tmpsave, c("SBS.RDS", "ITGS.RDS")))
  cfg$datasets <- "ITGS"
  capture.output(mdi_harmonize_pipeline(cfg))
  # the import's own processing order, without PIM
  expect_equal(calls$steps, c("order", "order", "read", "class"))
  expect_false(file.exists(file.path(cfg$dirs$tmpsave, "ITGS.RDS")))
  expect_true(file.exists(file.path(cfg$dirs$tmpsave, "SBS.RDS")))
  expect_true(file.exists(file.path(cfg$dirs$tmpsave, "OLD.RDS")))

  calls$steps <- character(0)
  cfg$datasets <- "BR"
  capture.output(mdi_harmonize_pipeline(cfg))
  expect_equal(calls$steps, c("order", "order", "read", "class", "pim"))
})

test_that("without an import, MDIdatasets is reported as ignored", {
  calls <- mock_pipeline_steps()
  cfg <- make_pipeline_cfg(import = FALSE)
  cfg$datasets <- "SBS"
  expect_message(capture.output(mdi_harmonize_pipeline(cfg)), "MDIdatasets is ignored")
  expect_length(calls$steps, 0)
})

test_that("mdi_harmonize_pipeline connects to Oracle only when needed, and closes the connection", {
  calls <- mock_pipeline_steps(formats = c("csv", "oracle"))
  capture.output(mdi_harmonize_pipeline(make_pipeline_cfg(db_user = "me")))
  expect_equal(calls$steps, c("connect", "read", "class", "pim", "close"))
  expect_equal(calls$channel, "channel")

  calls <- mock_pipeline_steps(formats = "csv")
  capture.output(mdi_harmonize_pipeline(make_pipeline_cfg(db_user = "me")))
  expect_false("connect" %in% calls$steps)
})

test_that("mdi_harmonize_pipeline uses a given connection and leaves it open", {
  calls <- mock_pipeline_steps(formats = "oracle")
  capture.output(mdi_harmonize_pipeline(make_pipeline_cfg(db_user = "me"), oracle_channel = "mine"))
  expect_false(any(c("connect", "close") %in% calls$steps))
  expect_equal(calls$channel, "mine")
})

test_that("mdi_harmonize_pipeline closes the Oracle connection when the import fails", {
  calls <- mock_pipeline_steps(formats = "oracle")
  testthat::local_mocked_bindings(mdi_read_firmdata = function(...) stop("import failed"))
  expect_error(capture.output(mdi_harmonize_pipeline(make_pipeline_cfg(db_user = "me"))), "import failed")
  expect_true("close" %in% calls$steps)
})

test_that("mdi_run writes the output description and runs the checks only before the modules", {
  out <- paste0(tempdir(), "/run_out/")
  dir.create(out, showWarnings = FALSE)
  checked <- new.env()
  checked$n <- 0
  testthat::local_mocked_bindings(
    mdi_harmonize_pipeline = function(cfg, oracle_channel) cfg,
    mdi_post_harmonization_checks = function(cfg) checked$n <- checked$n + 1
  )
  cfg <- list(dirs = list(output = out), country = "FI", launch_version = "v2.4",
              rocket_version = list(commit_hash = "abc"), payload_version = list(commit_hash = "def"),
              flags = list(module_run = FALSE, phc_run = TRUE))
  mdi_run(cfg)
  header <- readLines(paste0(out, "OutputDescription.csv"))
  expect_true(any(grepl("MDI launch version= v2.4", header)))
  expect_true(any(grepl("rocket version = abc", header)))
  expect_true(file.exists(paste0(out, "WarningsDescription.txt")))
  expect_equal(checked$n, 1)

  cfg$flags$module_run <- TRUE
  mdi_run(cfg)
  cfg$flags$module_run <- FALSE
  cfg$flags$phc_run <- FALSE
  mdi_run(cfg)
  expect_equal(checked$n, 1)
})
