#' Export Data and Log Output Description
#'
#' @description
#' Exports a given data table to a specified format (csv, RDS, txt, dta, xlsx,
#' sas) and appends an entry to an output description file.
#'
#' @param DT A `data.table` to be exported.
#' @param format Character. Export format: `"csv"`, `"RDS"`, `"txt"`,
#'   `"dta"`, `"xlsx"`, or `"sas"`.
#' @param output_name Character. File name to create, without extension or
#'   country code prefix.
#' @param datasets_used Character vector. Name(s) of the MD datasets used.
#' @param purpose Character. Research purpose of the analysis.
#' @param share_0_1 Character. Explanation of any shares equal to 0 or 1.
#' @param zeroes Character. Explanation of any zero values, or justification
#'   for suppressing them.
#' @param selection Character. Whether results cover the full sample or a
#'   selection.
#' @param dirOUTPUT Character. Path to the output directory (must end with
#'   `"/"`).
#' @param hier Logical. `TRUE` if the dataset has multiple aggregation levels
#'   (hierarchical structure). Default `FALSE`.
#' @param hhfile A `data.table` hierarchy file, required when `hier = TRUE`.
#'   Default `NULL`.
#' @param rel_other_output Character. How this output relates to other
#'   exported files. Default `""`.
#' @param export_type Character. Type of output: `"sum_stat"`, `"reg_tab"`,
#'   or `"other"`. Default `"sum_stat"`.
#' @param description Character. Additional explanation of the output file.
#'   Default `" "`.
#' @param CountryCode Character. Two-letter ISO country code used in file
#'   naming and the output description log. Default `"XX"`.
#' @param res_group Character. Research group identifier used in file naming.
#'   Default `""`.
#' @param show_values Numeric. `1` to show disclosed values, `0` to mask
#'   them. Default `0`.
#' @param show_domPerc Numeric. `1` to include dominance percentile columns
#'   in output, `0` to remove them. Default `1`.
#' @param minNumObs Integer. Minimum number of observations for disclosure
#'   checks, country-specific (`MDIminNumObs`). Passed to [`mdi_disclose()`] for
#'   `"sum_stat"` and [`mdi_disclose_reg_tab()`] for `"reg_tab"`. Default `5L`.
#' @param domSh Numeric. Dominance-share threshold for `"sum_stat"` exports,
#'   country-specific (`MDIdomSh`). Passed to [`mdi_disclose()`]. Default `0.85`.
#' @param domAllCols Numeric `0`/`1` for `"sum_stat"` exports, country-specific
#'   (`MDIdomAllCols`). `0` (default) masks `sum`-columns only; `1` masks every
#'   statistic derived from the dominance variable. Passed to [`mdi_disclose()`].
#' @param dom_direction Character. Dominance suppression direction for
#'   `"sum_stat"` exports. `"high"` (default) suppresses when
#'   `domPerc >= domSh`; `"low"` suppresses when `domPerc < domSh`.
#'   Passed to [`mdi_disclose()`].
#' @param disc_method Character. Disclosure rule for `"reg_tab"` exports.
#'   `"obs_df"` (default) uses `df` and `NumObs`; `"firm_count"` uses
#'   `NumFirms` and `NumEnt`. Passed to [`mdi_disclose_reg_tab()`].
#'
#' @return Writes files to disk; returns `NULL` invisibly.
#'
#' @examples
#' \dontrun{
#' mdi_export(
#'   DT          = my_agg_table,
#'   format      = "csv",
#'   output_name = "Coverage_Firms",
#'   datasets_used = c("BR", "SBS"),
#'   purpose     = "Number of firms by industry and year.",
#'   share_0_1   = "None",
#'   zeroes      = "None",
#'   selection   = "Full sample",
#'   export_type = "sum_stat",
#'   CountryCode = "NL",
#'   res_group   = "RDC1",
#'   dirOUTPUT   = "/output/NL/"
#' )
#' }
#'
#' @export

mdi_export <-
  function(DT,
           format,
           output_name,
           datasets_used,
           purpose,
           share_0_1,
           zeroes,
           selection,
           dirOUTPUT,
           hier = FALSE,
           hhfile = NULL,
           rel_other_output = "",
           export_type = "sum_stat",
           description = " ",
           CountryCode = "XX",
           res_group = "",
           show_values = 0,
           show_domPerc = 1,
           minNumObs = 5L,
           domSh = 0.85,
           domAllCols = 0,
           dom_direction = c("high", "low"),
           disc_method = c("obs_df", "firm_count")) {

    dom_direction <- match.arg(dom_direction)
    disc_method   <- match.arg(disc_method)
    check_choice(export_type, "export_type", c("sum_stat", "reg_tab", "other"))
    # "other" is for arbitrary content (e.g. plain text via format = "txt"),
    # so only sum_stat/reg_tab -- which rely on DT being tabular -- are checked.
    if (export_type != "other") check_dt(DT)
    valid_formats <- c("csv", "RDS", "txt", "dta", "xlsx", "sas")
    check_choice(format, "format", valid_formats)
    if (missing(dirOUTPUT)) stop("mdi_export(): 'dirOUTPUT' is required, the folder the file goes to.")
    check_string(dirOUTPUT, "dirOUTPUT")
    if (!grepl("[/\\\\]$", dirOUTPUT)) stop("mdi_export(): 'dirOUTPUT' must end with '/': \"", dirOUTPUT, "\".")

    if (export_type == "sum_stat") {
      vars     <- paste(attr(DT, "sumvars"), collapse = "; ")
      bygroups <- paste(attr(DT, "sorted"),  collapse = "; ")
    } else {
      vars     <- ""
      bygroups <- ""
    }

    if (export_type == "reg_tab") {
      disclosed <- mdi_disclose_reg_tab(DT, min_obs = minNumObs,
                                    show_disclosed = show_values,
                                    disc_method = disc_method)
      DT       <- disclosed$DT
      vars     <- disclosed$vars
      bygroups <- "model"
      if (disclosed$redacted_n > 0 && show_values != 1) {
        message(disclosed$redacted_n,
                " row(s) masked due to failing disclosure thresholds (df or NumObs).")
      }
    }

    # every field quoted, quotes inside a field doubled (valid CSV)
    newentry <- data.table(CountryCode, res_group, output_name, export_type, vars, bygroups,
                           paste0(datasets_used, collapse = ";"), purpose, share_0_1, zeroes,
                           rel_other_output, selection, description, dirOUTPUT)
    fwrite(newentry, paste0(dirOUTPUT, "OutputDescription.csv"), append = TRUE, col.names = FALSE, na = "NA",
           quote = TRUE)

    if (export_type == "sum_stat" && show_values != 1) {
      DT <- mdi_disclose(DT, hier = hier, hhfile = hhfile,
                     minNumObs = minNumObs, domSh = domSh, domAllCols = domAllCols,
                     dom_direction = dom_direction)
    }

    if (any(show_domPerc == 0 & grepl("domPerc", names(DT)))) {
      # a new table: the caller's keeps its domPerc columns
      DT <- DT[, !grepl("domPerc", names(DT)), with = FALSE]
    }

    file <- paste0(dirOUTPUT, CountryCode, "_", res_group, "_",
                   output_name, ".", format)
    if (format == "csv") {
      fwrite(DT, file)
    } else if (format == "RDS") {
      saveRDS(DT, file)
    } else if (format == "txt") {
      write.table(DT, file, sep = "\t")
    } else if (format == "dta") {
      haven::write_dta(DT, file)
    } else if (format == "xlsx") {
      openxlsx::write.xlsx(DT, file)
    } else if (format == "sas") {
      haven::write_sas(DT, file)
    }

    invisible(NULL)
  }
