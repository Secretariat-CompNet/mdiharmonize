#' Harmonize a classification code over time
#'
#' @description
#' Classification codes (e.g. CN8 product codes, PRODCOM codes, NUTS
#' regions) change from year to year. Given a year-on-year concordance of old
#' to new codes, builds the mapping from each year's codes to the codes of the
#' last year with [mdi_make_conc()], and applies it to `dt`: the harmonized
#' code replaces `code_name`, and the original is kept as `NSI_<code_name>`.
#'
#' @param dt A `data.table` with a `year` column and a `code_name` column.
#' @param conc A `data.table`, the year-on-year concordance
#'   (`<country>_<code>t0_<code>t1_conc.csv`): `left` (old code), `right`
#'   (new code), `year`.
#' @param code_name Character, the code column to harmonize (e.g. `"cn08"`).
#' @param data_years Numeric, length 2: the first and last year the raw data
#'   covers. Harmonization runs over the years both the concordance and the
#'   data cover (from the year after the data's first year, since the first
#'   year has no earlier year to map from).
#'
#' @return A list: `data`, `dt` with the harmonized `code_name` and the
#'   original as `NSI_<code_name>`; and `codes`, the mapping table
#'   [mdi_make_conc()] produced (`year`, `code_name`,
#'   `<code_name>_harmonized`). `dt` itself is not modified. `NULL`, with a
#'   warning, when the concordance's years and the data's don't overlap.
#'
#' @examples
#' # code A (2010) became B in 2011, which became C in 2012; X never changed
#' conc <- data.table::data.table(left = c("A", "X", "B", "X"), right = c("B", "X", "C", "X"),
#'                                year = c(2011, 2011, 2012, 2012))
#' dt <- data.table::data.table(year = c(2010, 2011, 2012, 2012), cn08 = c("A", "B", "C", "X"))
#' out <- mdi_harmonize_codes_over_time(dt, conc, "cn08", data_years = c(2010, 2012))
#' out$data
#' @export
mdi_harmonize_codes_over_time <- function(dt, conc, code_name, data_years) {
  check_string(code_name, "code_name")
  check_dt(dt, c("year", code_name), "dt")
  check_dt(conc, c("left", "right", "year"), "conc")
  if (!is.numeric(data_years) || length(data_years) != 2L) {
    stop("'data_years' must be two numbers: the first and last year of the data.")
  }

  conc_years <- range(conc[["year"]], na.rm = TRUE)
  first <- max(conc_years[1], data_years[1] + 1)
  last <- min(conc_years[2], data_years[2])
  # a:b would count down
  if (first > last) {
    warning("mdi_harmonize_codes_over_time(): the ", code_name, " concordance (", conc_years[1], "-",
            conc_years[2], ") doesn't overlap the data's years after the first (", data_years[1] + 1, "-",
            data_years[2], "), so there is nothing to harmonize.", call. = FALSE)
    return(NULL)
  }
  year_list <- first:last
  codes <- mdi_make_conc(conc, year_list, code_name)

  # match on the code as mdi_make_conc() cleans it (no spaces or dots);
  # the original code is kept as NSI_<code_name>
  data <- data.table::copy(dt)
  # cleaned once per distinct code, not per row
  x <- data[[code_name]]
  distinct <- unique(x)
  data.table::set(data, j = ".code_key", value = gsub("[ .]", "", distinct)[match(x, distinct)])
  codes_key <- data.table::setnames(data.table::copy(codes), code_name, ".code_key")
  data <- merge(data, codes_key, by = c("year", ".code_key"), all.x = TRUE)
  data[, (".code_key") := NULL]
  data.table::setcolorder(data, c("year", code_name))
  data.table::setnames(data,
                       old = c(code_name, paste0(code_name, "_harmonized")),
                       new = c(paste0("NSI_", code_name), code_name))

  list(data = data, codes = codes)
}
