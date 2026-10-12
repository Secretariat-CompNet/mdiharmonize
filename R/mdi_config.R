#' Build the MDI pipeline configuration object
#'
#' @description
#' Constructs the `cfg` object the pipeline functions take: the run's
#' directories, flags and versions. Derives the rocket/payload/metadata
#' directory paths from `mdi_dir`/`country`/`launch_version`, scans the
#' payload for the launch's onboarded countries and research modules, and
#' reads the rocket/payload version stamps. In MDI, the settings come from
#' the countdown script (their names there in brackets below).
#'
#' @param mdi_dir Character, the root MDI installation directory (`dirMDI`).
#' @param country Character, 2-3 letter NSI country code (`CountryCode`).
#' @param launch_version Character, e.g. `"v2.4"` (`MDI_launch_version`).
#' @param input_dir Character, directory holding the NSI/mock firm-level data
#'   files (`dirINPUTDATA`).
#' @param output_dir Character, directory generated files are exported to
#'   (`dirOUTPUT`).
#' @param tmp_dir Character, directory for temporary MDI virtual dataset
#'   storage (`dirTMPSAVE`), used as given (MDI's countdown includes the
#'   country, so switching country can't mix two countries' datasets).
#' @param import Logical, import raw NSI data files if `TRUE`; if `FALSE`,
#'   read the already-harmonized MDI virtual dataset from `tmp_dir` instead
#'   (`MDIimportFlag`).
#' @param clean_tmp Logical, clean `tmp_dir` before running (`MDIcleanTMP`).
#' @param phc_run Logical, `FALSE` to skip the post-harmonization report
#'   ([mdi_post_harmonization_checks()]), e.g. for mock data or CI
#'   (`MDIphcRUN`). Default `TRUE`.
#' @param module_run Logical, `TRUE` once post-harmonization checks have
#'   passed and modules are ready to run; `FALSE` when running a launch for
#'   the first time (`MDImoduleRUN`).
#' @param debug Logical, `TRUE` to leave logs/warnings/errors unsuppressed
#'   (`MDIdebug`).
#' @param filtered_harmonization Logical, harmonize only the variables listed
#'   in the current launch's `MDnames_select` file (`filteredHarmonization`).
#' @param db_user Character or `NULL`, database user for NSI datafiles with
#'   `format == "oracle"` (`db_user`, if set).
#' @param datasets Character vector or `NULL`, the MD datasets to harmonize
#'   (`MDIdatasets`, if set), e.g. `c("SBS", "PRODCOM")` to redo only those
#'   after a run that stopped there; the other datasets already in `tmp_dir`
#'   stay as they are. `NULL` (default): all. Checked against the country's
#'   concordance by [mdi_harmonize_pipeline()].
#' @param modules Character vector or `NULL`, the research modules to run
#'   (`MDImodules`, if set), e.g. `"MOD0"`. `NULL` (default): every module of
#'   the launch. With `filtered_harmonization`, only the chosen modules'
#'   variables are harmonized.
#' @param modules_dir Character or `NULL`, a folder of research modules to
#'   use instead of the launch's `Rmodules/` (`dirMODULES`, if set), e.g. a
#'   workshop participant's own module next to a shared, read-only MDI
#'   installation. Each 4-letter-named subfolder is a module. `NULL`
#'   (default): the launch's modules. The modules are run by the launch's
#'   `Rmodules/main.R` either way.
#'
#' @return A list (`cfg`) with elements `country`, `launch_version`, `dirs`
#'   (all derived paths: `mdi`, `rocket`, `control`, `payload`, `nsimeta`,
#'   `mdmeta`, `progs`, `output`, `tmpsave`, `inputdata`), `flags` (`import`,
#'   `clean_tmp`, `phc_run`, `module_run`, `debug`, `filtered_harmonization`),
#'   `db_user`, `datasets` (as given), `countries` (onboarded country codes
#'   found under the launch's `MDmetadata/`), `modules` (the research modules
#'   to run: those found under the launch's `Rmodules/` — a 4-letter-named
#'   subfolder is a module — narrowed to `modules` if given),
#'   `rocket_version`, `payload_version`. `dirs$progs` is the module folder
#'   (`modules_dir` if given), `dirs$main` the launch's `Rmodules/main.R`.
#'
#' @examples
#' mdi_dir <- paste0(tempdir(), "/")
#' dir.create(paste0(mdi_dir, "rocket"), recursive = TRUE)
#' dir.create(paste0(mdi_dir, "payload/Launch_v2.4/MDmetadata/FI"), recursive = TRUE)
#' dir.create(paste0(mdi_dir, "payload/Launch_v2.4/Rmodules/CPNT"), recursive = TRUE)
#' writeLines("commit\nabc123", paste0(mdi_dir, "rocket/rocket_github_commit.csv"))
#' writeLines("commit\ndef456", paste0(mdi_dir, "payload/Launch_v2.4/payload_github_commit.csv"))
#' cfg <- mdi_config(
#'   mdi_dir = mdi_dir, country = "FI", launch_version = "v2.4",
#'   input_dir = mdi_dir, output_dir = mdi_dir, tmp_dir = mdi_dir,
#'   phc_run = FALSE
#' )
#' cfg$countries
#' cfg$modules
#' @export
mdi_config <- function(mdi_dir, country, launch_version,
                        input_dir, output_dir, tmp_dir,
                        import = TRUE, clean_tmp = FALSE, phc_run = TRUE,
                        module_run = FALSE, debug = FALSE,
                        filtered_harmonization = FALSE,
                        db_user = NULL, datasets = NULL, modules = NULL,
                        modules_dir = NULL) {
  check_string(country, "country")
  check_string(launch_version, "launch_version")
  flags <- list(import = import, clean_tmp = clean_tmp, phc_run = phc_run, module_run = module_run,
                debug = debug, filtered_harmonization = filtered_harmonization)
  # read with isTRUE(): a 1 or "TRUE" would silently count as FALSE
  for (f in names(flags)) {
    v <- flags[[f]]
    if (!is.logical(v) || length(v) != 1L || is.na(v)) {
      stop("mdi_config(): '", f, "' must be TRUE or FALSE.", call. = FALSE)
    }
  }
  .check_dir(mdi_dir, "mdi_dir")
  .check_dir(paste0(mdi_dir, "rocket/"), "mdi_dir")
  .check_dir(paste0(mdi_dir, "payload/Launch_", launch_version, "/"), "mdi_dir")
  .check_dir(output_dir, "output_dir")
  .check_dir(tmp_dir, "tmp_dir")
  # an NSI whose data is all in Oracle may not use it
  .check_dir(input_dir, "input_dir", missing = "warn")
  if (!is.null(modules_dir)) .check_dir(modules_dir, "modules_dir")
  if (!is.null(datasets)) check_char_vec(datasets, "datasets")
  if (!is.null(modules)) check_char_vec(modules, "modules")

  dirs <- list(
    mdi       = mdi_dir,
    rocket    = paste0(mdi_dir, "rocket/"),
    control   = paste0(mdi_dir, "rocket/control/"),
    payload   = paste0(mdi_dir, "payload/Launch_", launch_version, "/"),
    nsimeta   = paste0(mdi_dir, "rocket/NSImetadata/", country, "/"),
    mdmeta    = paste0(mdi_dir, "payload/Launch_", launch_version, "/MDmetadata/"),
    progs     = if (is.null(modules_dir)) paste0(mdi_dir, "payload/Launch_", launch_version, "/Rmodules/") else modules_dir,
    main      = paste0(mdi_dir, "payload/Launch_", launch_version, "/Rmodules/main.R"),
    output    = output_dir,
    tmpsave   = tmp_dir,
    inputdata = input_dir
  )

  ctry_folders <- list.dirs(dirs$mdmeta, full.names = FALSE, recursive = FALSE)
  countries <- ctry_folders[nchar(ctry_folders) %in% c(2, 3)]

  module_folders <- list.dirs(dirs$progs, full.names = FALSE, recursive = FALSE)
  found_modules <- module_folders[nchar(module_folders) == 4]
  if (!is.null(modules)) {
    unknown <- setdiff(modules, found_modules)
    if (length(unknown) > 0) {
      stop("mdi_config(): module(s) ", paste(unknown, collapse = ", "), " not in ", dirs$progs,
           ". The launch's modules: ", paste(found_modules, collapse = ", "), ".", call. = FALSE)
    }
    found_modules <- found_modules[found_modules %in% modules]
  }

  rocket_version <- utils::read.csv(paste0(dirs$rocket, "rocket_github_commit.csv"), header = TRUE)
  payload_version <- utils::read.csv(paste0(dirs$payload, "payload_github_commit.csv"), header = TRUE)

  list(
    country = country,
    launch_version = launch_version,
    dirs = dirs,
    flags = flags,
    db_user = db_user,
    datasets = datasets,
    countries = countries,
    modules = found_modules,
    rocket_version = rocket_version,
    payload_version = payload_version
  )
}

# A folder path: a non-empty string ending in a separator (paths are joined by
# pasting, so "out" + "FI.csv" would give "outFI.csv"), for a folder that
# exists.
.check_dir <- function(path, arg, missing = c("stop", "warn")) {
  missing <- match.arg(missing)
  check_string(path, arg)
  if (!grepl("[/\\\\]$", path)) {
    stop("mdi_config(): '", arg, "' must end with '/': \"", path, "\".", call. = FALSE)
  }
  if (!dir.exists(path)) {
    msg <- paste0("mdi_config(): the folder in '", arg, "' doesn't exist: \"", path, "\".")
    if (missing == "stop") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
}
