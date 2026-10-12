#' Write the post-harmonization quality report
#'
#' @description
#' The review gate between harmonization and the modules: checks every
#' harmonized MD dataset in `cfg$dirs$tmpsave` with
#' [mdi_check_harmonized()] (duplicate keys, rows without a year, classes and
#' date formats, date ranges, breaks in yearly statistics, jumps in the yearly
#' share of `NA` values) and adds the
#' coercion fallout log (NAs introduced, or out-of-domain values force-cast to
#' logical, by the type conversion). Writes, in `cfg$dirs$tmpsave`:
#' \itemize{
#'   \item `<CC>_phc_results.txt`, all results;
#'   \item `breaks_report.pdf`, a page per variable either check flags: its
#'     yearly statistics ([mdi_plot_breaks()], numeric variables) above its
#'     yearly share of `NA` values ([mdi_plot_na_share()]); needs ggplot2 and
#'     gridExtra.
#' }
#' Every numeric variable a dataset has is checked, including the ones the
#' pipeline computes (`K_pim`), so run this after the whole pipeline.
#' Someone at the NSI reviews these; once they look fine, `MDImoduleRUN =
#' TRUE` lets the modules run.
#'
#' @param cfg config list from [mdi_config()], with the MD metadata
#'   (`cfg$md`) and the selected NSI metadata (`cfg$nsi`) loaded.
#' @param threshold numeric(1), the relative year-on-year change flagged as
#'   a break (see [mdi_check_harmonized()]).
#' @param na_threshold numeric(1), the year-on-year change in the share of
#'   `NA` values flagged (see [mdi_check_harmonized()]).
#'
#' @return The per-dataset results of [mdi_check_harmonized()], invisibly.
#' @examples
#' # a harmonized SBS with a duplicated firm-year and a row without a year
#' tmpsave <- paste0(tempfile("phc_"), "/")
#' dir.create(tmpsave)
#' saveRDS(data.table::data.table(firmid = c("a", "a", "b"), year = c(2020L, 2020L, NA),
#'                                emp = c(1, 1, 1)),
#'         paste0(tmpsave, "SBS.RDS"))
#' catalogue <- data.table::data.table(
#'   MD_dataset = "SBS", MD_varname = c("firmid", "year", "emp"), is_key = c(1L, 1L, 0L),
#'   class = c("character", "integer", "numeric"), domain = "", FI = "2020 - 2021")
#' cfg <- list(dirs = list(tmpsave = tmpsave), country = "FI",
#'             md = list(MD_catalogue = catalogue,
#'                       MD_varnames = catalogue[, c("MD_dataset", "MD_varname")]),
#'             nsi = list())
#' res <- mdi_post_harmonization_checks(cfg)
#' res$SBS$duplicates
#' readLines(paste0(tmpsave, "FI_phc_results.txt"))[1:8]
#' @export
mdi_post_harmonization_checks <- function(cfg, threshold = 0.25, na_threshold = 0.1) {
  check_cfg(cfg, need = c("md", "nsi"))
  tmpsave <- cfg$dirs$tmpsave
  results_file <- paste0(cfg$country, "_phc_results.txt")
  breaks_pdf <- paste0(tmpsave, "breaks_report.pdf")
  log_phc <- function(message) log_warnings(message, tmpsave, append = TRUE, file_name = results_file)
  log_table <- function(tbl) log_phc(utils::capture.output(print(tbl)))

  # catalogue rows in MD_varnames' order (key variables first), the order the
  # results list variables in
  var_id <- function(md) paste(md$MD_dataset, md$MD_varname)
  catalogue <- cfg$md$MD_catalogue[match(var_id(cfg$md$MD_varnames), var_id(cfg$md$MD_catalogue))]
  datasets <- unique(catalogue$MD_dataset)
  datasets <- datasets[file.exists(paste0(tmpsave, datasets, ".RDS"))]
  phc <- lapply(stats::setNames(datasets, datasets), function(ds) {
    mdi_check_harmonized(readRDS(paste0(tmpsave, ds, ".RDS")), ds, catalogue,
                         cfg$country, threshold = threshold, na_threshold = na_threshold)
  })
  phc_section <- function(name) rbindlist(lapply(phc, `[[`, name), fill = TRUE)

  log_warnings("Post_Harmonization_Check_Results\n--------------\n", tmpsave, append = FALSE,
               file_name = results_file)

  log_phc("\n--- Duplicate Check Summary ---\n")
  log_table(phc_section("duplicates"))

  # a dataset or rows without a year: harmonization lost it
  years <- phc_section("years")
  log_phc("\n--- Year Check Summary (every row needs a year) ---\n")
  no_year <- years[years[["num_rows_no_year"]] > 0]
  if (nrow(no_year) > 0) {
    log_table(no_year)
  } else {
    log_phc("Every row of every dataset has a year.\n")
  }

  # only the mismatches: the full table (every variable) is too long to read
  # and gets cut when printed
  classes <- phc_section("classes")
  log_phc("\n--- Variable Class Check Summary (variables whose class doesn't match) ---\n")
  unmatched <- !classes[["class_match"]] %in% TRUE
  # declared factor without an MD_codebook domain: mdi_coerce_md_types()
  # leaves these unconverted on purpose, so not a mismatch to chase
  domain <- catalogue$domain[match(paste(classes$dataset, classes$variable), var_id(catalogue))]
  by_design <- unmatched & tolower(classes[["expected_class"]]) %in% "factor" &
    !(tolower(trimws(domain)) %in% "md_codebook")
  mismatch <- classes[unmatched & !by_design]
  if (nrow(mismatch) > 0) {
    log_table(mismatch)
  } else {
    log_phc(paste0(nrow(classes), " variables checked, all classes match",
                   if (any(by_design)) " apart from the ones below", ".\n"))
  }
  if (any(by_design)) {
    log_phc(paste0("Declared factor but left as they are on purpose (their domain isn't MD_codebook, ",
                   "so their codes aren't checked against a codebook): ",
                   paste(classes$dataset[by_design], classes$variable[by_design], collapse = ", "), ".\n"))
  }

  # Once coercion is universal the class check above reads TRUE by
  # construction; this is what catches silent data loss (as.numeric()/as.Date()
  # turning unparseable values into NA) and silent corruption (as.logical()
  # collapsing a 3-valued code like 0/1/2 into TRUE/FALSE).
  fallout_log <- paste0(tmpsave, "coercion_fallout_log.csv")
  if (file.exists(fallout_log)) {
    fallout <- fread(fallout_log)
    fallout <- fallout[fallout[["na_introduced"]] > 0 | fallout[["logical_out_of_domain"]] > 0]
    log_phc("\n--- Coercion Fallout Summary (NAs introduced or out-of-domain values force-cast) ---\n")
    if (nrow(fallout) > 0) {
      log_table(fallout)
    } else {
      log_phc("No NAs introduced and no out-of-domain values force-cast during type conversion.\n")
    }
  } else {
    log_phc("\n--- Coercion Fallout Summary ---\nNo coercion_fallout_log.csv found (no data import in this run or an earlier one).\n")
  }

  log_phc("\n--- Date Format & Class Check Summary ---\n")
  log_table(phc_section("date_formats"))

  log_phc("\n--- Detected Date Format and Ranges Summary ---\n")
  log_table(phc_section("date_ranges"))

  breaks <- phc_section("breaks")
  na_shares <- phc_section("na_shares")
  if (nrow(breaks) > 0 || nrow(na_shares) > 0) {
    if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("ggplot2", quietly = TRUE)) {
      stop("mdi_post_harmonization_checks(): the break plots need the ggplot2 and gridExtra packages.")
    }
    # building the pages opens a graphics device: no device, so no stray
    # Rplots.pdf in the working directory
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    pages <- unlist(lapply(names(phc), function(ds) {
      vars <- union(names(phc[[ds]]$series), names(phc[[ds]]$na_series))
      lapply(vars, function(v) .phc_page(phc[[ds]], ds, v))
    }), recursive = FALSE)
    ggplot2::ggsave(breaks_pdf, gridExtra::marrangeGrob(grobs = pages, nrow = 1, ncol = 1, top = NULL),
                    width = 9, height = 8)
    log_phc(paste("\nPlots of the flagged variables saved to:", breaks_pdf, "\n"))
  }
  if (nrow(breaks) > 0) {
    log_phc("\n--- Break Summary Table (statistics of the non-NA values) ---\n")
    log_table(breaks)
  } else {
    log_phc("\n--- Break Detection: No aggregate-level breaks found. ---\n")
  }
  log_phc(paste0("\n--- NA Share Check (change of more than ", na_threshold * 100,
                 " percentage points from the year before, or every value NA in a year) ---\n"))
  if (nrow(na_shares) > 0) {
    # a variable NA in every year: one line, not a row per year
    vars <- unique(na_shares[, c("dataset", "variable")])
    is_always <- mapply(function(ds, v) all(phc[[ds]]$na_series[[v]][["na_share"]] == 1),
                        vars[["dataset"]], vars[["variable"]])
    always <- vars[is_always]
    if (nrow(always) > 0) {
      log_phc(paste0("NA in every year: ", paste(always[["dataset"]], always[["variable"]], collapse = ", "), ".\n"))
    }
    rest <- na_shares[!always, on = c("dataset", "variable")]
    if (nrow(rest) > 0) {
      rest[, c("na_share", "change") := lapply(.SD, round, 3), .SDcols = c("na_share", "change")]
      log_table(rest)
    }
  } else {
    log_phc("No variable's share of NA values jumps, and none is NA in every row of a year.\n")
  }

  message("\nPOST-HARMONIZATION CHECK COMPLETE\n",
          sprintf("Results saved in:\n- %s\n- %s\n", paste0(tmpsave, results_file), breaks_pdf),
          "Please review:\n",
          "1. TXT file  \u2192 Duplicates, class & date format issues, and date ranges\n",
          "2. PDF file  \u2192 Plots of the variables with a break or a jump in their share of NAs\n",
          "Resolve any flagged issues before proceeding by setting MDImoduleRUN = TRUE.")
  invisible(phc)
}

# One PDF page per flagged variable: its yearly statistics (numeric variables)
# above its yearly share of NA values (non-key variables).
.phc_page <- function(res, ds, v) {
  grobs <- list()
  if (!is.null(res$series[[v]])) {
    grobs$stats <- ggplot2::ggplotGrob(.phc_stats_plot(res, v))
  }
  if (!is.null(res$na_series[[v]])) {
    na_rows <- if (is.null(res$na_shares)) data.table(year = numeric()) else res$na_shares[res$na_shares[["variable"]] == v]
    grobs$na <- ggplot2::ggplotGrob(mdi_plot_na_share(res$na_series[[v]], na_rows, title = "Share of NA values"))
  }
  gridExtra::arrangeGrob(grobs = grobs, ncol = 1, heights = if (length(grobs) == 2) c(3, 2) else 1,
                         top = paste(ds, "-", v))
}

.phc_stats_plot <- function(res, v) {
  break_rows <- if (is.null(res$breaks)) {
    data.table(dataset = character(), variable = character(), stat = character(), year = numeric())
  } else {
    res$breaks[res$breaks[["variable"]] == v]
  }
  mdi_plot_breaks(res$series[[v]], break_rows, title = "Statistics of the non-NA values")
}
