td <- paste0(tempdir(), "/")

make_fixture_dir <- function() {
  mdi_dir <- paste0(tempfile("mdi_"), "/")
  dir.create(paste0(mdi_dir, "rocket"), recursive = TRUE)
  dir.create(paste0(mdi_dir, "payload/Launch_v2.4/MDmetadata/FI"), recursive = TRUE)
  dir.create(paste0(mdi_dir, "payload/Launch_v2.4/MDmetadata/DE"), recursive = TRUE)
  dir.create(paste0(mdi_dir, "payload/Launch_v2.4/MDmetadata/tools"), recursive = TRUE)
  dir.create(paste0(mdi_dir, "payload/Launch_v2.4/Rmodules/CPNT"), recursive = TRUE)
  dir.create(paste0(mdi_dir, "payload/Launch_v2.4/Rmodules/utils"), recursive = TRUE)
  writeLines(c("commit", "abc123"), paste0(mdi_dir, "rocket/rocket_github_commit.csv"))
  writeLines(c("commit", "def456"), paste0(mdi_dir, "payload/Launch_v2.4/payload_github_commit.csv"))
  mdi_dir
}

test_that("mdi_config derives all directory paths correctly", {
  mdi_dir <- make_fixture_dir()
  io <- paste0(tempfile("io_"), "/")
  for (d in c("in/", "out/", "tmp/")) dir.create(paste0(io, d), recursive = TRUE)
  cfg <- mdi_config(
    mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
    input_dir = paste0(io, "in/"), output_dir = paste0(io, "out/"), tmp_dir = paste0(io, "tmp/")
  )
  expect_equal(cfg$dirs$mdi, mdi_dir)
  expect_equal(cfg$dirs$rocket, paste0(mdi_dir, "rocket/"))
  expect_equal(cfg$dirs$payload, paste0(mdi_dir, "payload/Launch_v2.4/"))
  expect_equal(cfg$dirs$nsimeta, paste0(mdi_dir, "rocket/NSImetadata/FI/"))
  expect_equal(cfg$dirs$mdmeta, paste0(mdi_dir, "payload/Launch_v2.4/MDmetadata/"))
  expect_equal(cfg$dirs$progs, paste0(mdi_dir, "payload/Launch_v2.4/Rmodules/"))
  expect_equal(cfg$dirs$output, paste0(io, "out/"))
  expect_equal(cfg$dirs$inputdata, paste0(io, "in/"))
})

test_that("mdi_config scans onboarded countries and research modules", {
  mdi_dir <- make_fixture_dir()
  cfg <- mdi_config(
    mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
    input_dir = td, output_dir = td, tmp_dir = td
  )
  expect_setequal(cfg$countries, c("FI", "DE"))
  expect_equal(cfg$modules, "CPNT")
})

test_that("mdi_config does not hardcode launch-specific country exclusions", {
  mdi_dir <- make_fixture_dir()
  dir.create(paste0(mdi_dir, "payload/Launch_v2.4/MDmetadata/PT"), recursive = TRUE)
  cfg <- mdi_config(
    mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
    input_dir = td, output_dir = td, tmp_dir = td
  )
  expect_setequal(cfg$countries, c("FI", "DE", "PT"))
})

test_that("mdi_config reads rocket/payload version stamps", {
  mdi_dir <- make_fixture_dir()
  cfg <- mdi_config(
    mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
    input_dir = td, output_dir = td, tmp_dir = td
  )
  expect_equal(cfg$rocket_version$commit, "abc123")
  expect_equal(cfg$payload_version$commit, "def456")
})

# countdown.R already appends the country subfolder for mock runs; appending
# it here too produced tmp/FI/FI/
test_that("mdi_config uses tmp_dir as given", {
  mdi_dir <- make_fixture_dir()
  tmp_fi <- paste0(tempfile("tmp_"), "/FI/")
  dir.create(tmp_fi, recursive = TRUE)
  for (phc in c(TRUE, FALSE)) {
    cfg <- mdi_config(
      mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
      input_dir = td, output_dir = td, tmp_dir = tmp_fi,
      phc_run = phc
    )
    expect_equal(cfg$dirs$tmpsave, tmp_fi)
  }
})

test_that("mdi_config stores all flags and db_user", {
  mdi_dir <- make_fixture_dir()
  cfg <- mdi_config(
    mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
    input_dir = td, output_dir = td, tmp_dir = td,
    import = FALSE, clean_tmp = TRUE, phc_run = FALSE, module_run = TRUE,
    debug = TRUE, filtered_harmonization = TRUE, db_user = "oracle_user"
  )
  expect_equal(cfg$flags, list(
    import = FALSE, clean_tmp = TRUE, phc_run = FALSE, module_run = TRUE,
    debug = TRUE, filtered_harmonization = TRUE
  ))
  expect_equal(cfg$db_user, "oracle_user")
  expect_equal(cfg$country, "FI")
  expect_equal(cfg$launch_version, "v2.4")
})

test_that("mdi_config validates its string arguments", {
  expect_error(mdi_config(1, "FI", "v2.4", "a", "b", "c"), "mdi_dir")
  expect_error(mdi_config("a", "", "v2.4", "a", "b", "c"), "country")
})

test_that("mdi_config checks its folders and flags", {
  mdi_dir <- make_fixture_dir()
  cfg_with <- function(...) {
    args <- utils::modifyList(list(mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
                                   input_dir = td, output_dir = td, tmp_dir = td), list(...))
    do.call(mdi_config, args)
  }
  expect_error(cfg_with(output_dir = tempdir()), "'output_dir' must end with '/'")
  expect_error(cfg_with(tmp_dir = paste0(td, "nope/")), "the folder in 'tmp_dir' doesn't exist")
  expect_error(cfg_with(launch_version = "v9.9"), "the folder in 'mdi_dir' doesn't exist: .*Launch_v9.9/")
  # input_dir may be unused (Oracle): a warning only
  expect_warning(cfg_with(input_dir = paste0(td, "nope/")), "the folder in 'input_dir' doesn't exist")
  # isTRUE() would read these as FALSE without a word
  expect_error(cfg_with(import = 1), "'import' must be TRUE or FALSE")
  expect_error(cfg_with(module_run = "TRUE"), "'module_run' must be TRUE or FALSE")
})

test_that("mdi_config narrows the modules to MDImodules, and stops on an unknown one", {
  mdi_dir <- make_fixture_dir()
  dir.create(paste0(mdi_dir, "payload/Launch_v2.4/Rmodules/MOD0"))
  args <- list(mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
               input_dir = td, output_dir = td, tmp_dir = td)
  expect_equal(do.call(mdi_config, args)$modules, c("CPNT", "MOD0"))
  expect_equal(do.call(mdi_config, c(args, modules = "MOD0"))$modules, "MOD0")
  expect_error(do.call(mdi_config, c(args, modules = "XXXX")),
               "module\\(s\\) XXXX not in .*The launch's modules: CPNT, MOD0")
  expect_error(do.call(mdi_config, c(args, list(modules = character(0)))), "non-empty character")
})

test_that("mdi_config keeps MDIdatasets as given, NULL meaning all", {
  mdi_dir <- make_fixture_dir()
  args <- list(mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
               input_dir = td, output_dir = td, tmp_dir = td)
  expect_null(do.call(mdi_config, args)$datasets)
  expect_equal(do.call(mdi_config, c(args, list(datasets = c("SBS", "BR"))))$datasets, c("SBS", "BR"))
  expect_error(do.call(mdi_config, c(args, list(datasets = 1))), "non-empty character")
})

test_that("mdi_config takes the modules from modules_dir, and main.R from the launch", {
  mdi_dir <- make_fixture_dir()
  mods <- paste0(tempfile("mods_"), "/")
  dir.create(paste0(mods, "ABCD"), recursive = TRUE)
  dir.create(paste0(mods, "notes"))
  args <- list(mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
               input_dir = td, output_dir = td, tmp_dir = td)
  cfg <- do.call(mdi_config, c(args, list(modules_dir = mods)))
  expect_equal(cfg$dirs$progs, mods)
  expect_equal(cfg$modules, "ABCD")
  expect_equal(cfg$dirs$main, paste0(mdi_dir, "payload/Launch_v2.4/Rmodules/main.R"))

  # without it, the launch's modules, as before
  cfg <- do.call(mdi_config, args)
  expect_equal(cfg$dirs$progs, paste0(mdi_dir, "payload/Launch_v2.4/Rmodules/"))
  expect_equal(cfg$modules, "CPNT")

  expect_error(do.call(mdi_config, c(args, list(modules_dir = paste0(tempfile(), "/")))), "modules_dir")
})
