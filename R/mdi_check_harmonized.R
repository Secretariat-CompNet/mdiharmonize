#' Check a harmonized MD dataset against its metadata
#'
#' @description
#' The post-harmonization quality checks for one MD dataset: whether its key
#' variables identify rows uniquely, whether every row has a `year`, whether
#' each variable has the class (and, for dates, the format) `md_catalogue`
#' declares, which dates the data actually covers, and whether yearly
#' statistics (p25, p50, p75, mean, sd) of its numeric variables change by
#' more than `threshold` from one year to the next, a possible break in the
#' series, and whether the share of `NA` values of any variable changes by
#' more than `na_threshold` from one year to the next or is 100% in a year
#' (the statistics only see the non-`NA` values, so a loss of coverage
#' doesn't show in them).
#'
#' It checks the data against the metadata, assuming the metadata itself is
#' well-formed; that is checked separately, on the metadata files alone
#' (MDI's metadata checker).
#'
#' @param dt data.table, the harmonized dataset.
#' @param dataset character(1), its MD dataset name, e.g. `"BR"`.
#' @param md_catalogue data.table, the MD catalogue: `MD_dataset`,
#'   `MD_varname`, `is_key`, `class`, `domain` (the date format, for date
#'   variables) and a coverage column per country.
#' @param country character(1), the country code; `md_catalogue[[country]]` is
#'   the expected date range reported next to the actual one.
#' @param break_vars character or `NULL`: the numeric variables to check for
#'   breaks. `NULL` checks every variable `md_catalogue` declares numeric for
#'   `dataset`; variables not declared numeric are skipped either way.
#' @param threshold numeric(1), the relative year-on-year change above which a
#'   statistic is flagged (0.25 = 25%; on real firm-level data, 10% flags
#'   about a third of all year-on-year changes). From a previous value of 0 there is
#'   no relative change: p25, p50 and p75 aren't flagged then (they jump
#'   between 0 and small values for variables with many zeros), a mean or sd
#'   that leaves 0 is, with `from_zero = TRUE` and `growth` `NA`.
#' @param na_threshold numeric(1), the year-on-year change in a variable's
#'   share of `NA` values above which it is flagged (0.1 = 10 percentage
#'   points).
#'
#' @return A list:
#' \describe{
#'   \item{duplicates}{one row: key variables, whether and how many rows are duplicated.}
#'   \item{years}{one row: whether there is a `year` column, and how many rows have no year.}
#'   \item{classes}{expected and actual class of each non-date variable.}
#'   \item{date_formats}{expected and actual class and format of each date variable.}
#'   \item{date_ranges}{detected format, first and last date, and expected range of each date variable.}
#'   \item{breaks}{the flagged statistics: `dataset`, `variable`, `stat`, `year`, `growth`, `from_zero`.}
#'   \item{na_shares}{the flagged years: `dataset`, `variable`, `year`, `na_share`, `change` (from the year before), `all_na`.}
#'   \item{series}{named list, per numeric variable flagged by either check, of its yearly statistics (input to [mdi_plot_breaks()]).}
#'   \item{na_series}{named list, per variable flagged by either check, of its yearly share of `NA` values: `year`, `na_share` (input to [mdi_plot_na_share()]).}
#' }
#' A table is `NULL` when no variable applies (e.g. `breaks` without a `year`
#' column or without any break). Key variables are left out of the `NA`
#' check.
#'
#' @examples
#' dt <- data.table::data.table(firmid = c("a", "a", "b"), year = c(2020, 2020, 2021),
#'                              emp = c(10, 12, 20))
#' catalogue <- data.table::data.table(
#'   MD_dataset = "SBS", MD_varname = c("firmid", "year", "emp"),
#'   is_key = c(1L, 1L, 0L), class = c("character", "numeric", "numeric"),
#'   domain = "", FI = "2020 - 2021")
#' res <- mdi_check_harmonized(dt, "SBS", catalogue, "FI")
#' res$duplicates
#' res$breaks
#' @export
mdi_check_harmonized <- function(dt, dataset, md_catalogue, country,
                                 break_vars = NULL, threshold = 0.25, na_threshold = 0.1) {
  check_dt(dt, arg_name = "dt")
  check_string(dataset, "dataset")
  check_string(country, "country")
  check_dt(md_catalogue, c("MD_dataset", "MD_varname", "is_key", "class", "domain", country),
           "md_catalogue")
  if (!is.null(break_vars) && !is.character(break_vars))
    stop("'break_vars' must be a character vector or NULL.")
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) || threshold < 0)
    stop("'threshold' must be a single non-negative number.")
  if (!is.numeric(na_threshold) || length(na_threshold) != 1L || is.na(na_threshold) || na_threshold < 0)
    stop("'na_threshold' must be a single non-negative number.")

  meta <- md_catalogue[md_catalogue[["MD_dataset"]] == dataset]
  classes <- .phc_classes(dt, dataset, meta)
  breaks <- .phc_breaks(dt, dataset, meta, break_vars, threshold)
  nas <- .phc_na_shares(dt, dataset, meta, na_threshold)
  flagged <- unique(c(breaks$breaks[["variable"]], nas$na_shares[["variable"]]))

  list(
    duplicates = .phc_duplicates(dt, dataset, meta),
    years = .phc_years(dt, dataset),
    classes = classes$classes,
    date_formats = classes$date_formats,
    date_ranges = .phc_date_ranges(dt, dataset, meta, country),
    breaks = breaks$breaks,
    na_shares = nas$na_shares,
    series = breaks$series[intersect(flagged, names(breaks$series))],
    na_series = nas$series[intersect(flagged, names(nas$series))]
  )
}

.phc_date_formats <- c("%Y-%m-%d", "%Y%m%d", "%d%m%Y", "%d/%m/%Y", "%m/%d/%Y", "%Y")

# The first format the first 10 non-missing values all parse with, and print
# back unchanged: the round trip keeps a year-only format ("%Y") from
# matching an ISO string like "1831-01-01" (it would parse only the year and
# take month/day from today's date).
.phc_detect_format <- function(x) {
  if (inherits(x, c("Date", "IDate"))) return("%Y-%m-%d")
  x <- utils::head(stats::na.omit(as.character(x)), 10)
  if (!length(x)) return("unknown")
  fits <- vapply(.phc_date_formats, function(fmt) {
    d <- as.Date(x, format = fmt)
    all(!is.na(d)) && all(format(d, fmt) == x)
  }, logical(1))
  if (any(fits)) .phc_date_formats[fits][1] else "unknown"
}

# TRUE when every non-missing value parses with `fmt`.
.phc_parses_as_date <- function(x, fmt) {
  if (inherits(x, c("Date", "IDate"))) return(TRUE)
  all(is.na(x) | !is.na(as.Date(as.character(x), format = fmt)))
}

.phc_duplicates <- function(dt, dataset, meta) {
  key_vars <- unique(meta[["MD_varname"]][meta[["is_key"]] == 1])
  # a partial key is checked on the key variables present
  present <- key_vars[key_vars %in% names(dt)]
  if (all(is.na(key_vars)) || all(key_vars == "") || !length(present)) {
    return(data.table(dataset = dataset, id_var = NA, has_duplicates = NA,
                      num_duplicated_rows = NA, num_unique_duplicated_keys = NA))
  }
  dup_keys <- dt[duplicated(dt, by = present), present, with = FALSE]
  data.table(
    dataset = dataset,
    id_var = paste(present, collapse = ", "),
    has_duplicates = nrow(dup_keys) > 0,
    num_duplicated_rows = nrow(dup_keys),
    num_unique_duplicated_keys = if (nrow(dup_keys) > 0) uniqueN(dup_keys) else 0
  )
}

# Every MD dataset is by year: a row without one can't be used, and a whole
# dataset without one means harmonization lost it
.phc_years <- function(dt, dataset) {
  has_year <- "year" %in% names(dt)
  data.table(dataset = dataset, has_year = has_year, num_rows = nrow(dt),
             num_rows_no_year = if (has_year) sum(is.na(dt[["year"]])) else nrow(dt))
}

# Date variables with a declared format go to date_formats, all others to
# classes.
.phc_classes <- function(dt, dataset, meta) {
  meta <- meta[meta[["MD_varname"]] %in% names(dt)]
  if (!nrow(meta)) return(list(classes = NULL, date_formats = NULL))

  vars <- meta[["MD_varname"]]
  exp_class <- meta[["class"]]
  exp_fmt <- meta[["domain"]]
  actual <- vapply(vars, function(v) class(dt[[v]])[1L], character(1), USE.NAMES = FALSE)
  class_match <- !is.na(exp_class) & tolower(exp_class) == tolower(actual)
  is_date <- !is.na(exp_class) & tolower(exp_class) == "date" & !is.na(exp_fmt) & exp_fmt != ""

  classes <- if (any(!is_date)) {
    data.table(dataset = dataset, variable = vars[!is_date],
               expected_class = exp_class[!is_date], actual_class = actual[!is_date],
               class_match = class_match[!is_date])
  }
  date_formats <- if (any(is_date)) {
    d <- which(is_date)
    data.table(
      dataset = dataset, variable = vars[d],
      expected_class = exp_class[d], actual_class = actual[d],
      expected_format = exp_fmt[d],
      actual_format = vapply(vars[d], function(v) .phc_detect_format(dt[[v]]), character(1),
                             USE.NAMES = FALSE),
      format_valid = mapply(function(v, fmt) .phc_parses_as_date(dt[[v]], fmt), vars[d], exp_fmt[d],
                            USE.NAMES = FALSE),
      class_match = class_match[d]
    )
  }
  list(classes = classes, date_formats = date_formats)
}

.phc_date_ranges <- function(dt, dataset, meta, country) {
  meta <- meta[tolower(meta[["class"]]) == "date" & meta[["MD_varname"]] %in% names(dt)]
  if (!nrow(meta)) return(NULL)

  rbindlist(lapply(seq_len(nrow(meta)), function(i) {
    x <- dt[[meta[["MD_varname"]][i]]]
    fmt <- .phc_detect_format(x)
    # already a Date: no re-parsing, which would drop month/day
    dates <- if (inherits(x, c("Date", "IDate"))) x
             else if (fmt != "unknown") as.Date(as.character(x), format = fmt)
    has_dates <- !is.null(dates) && !all(is.na(dates))
    data.table(
      dataset = dataset,
      variable = meta[["MD_varname"]][i],
      actual_format = fmt,
      actual_min_date = if (has_dates) min(dates, na.rm = TRUE) else NA,
      actual_max_date = if (has_dates) max(dates, na.rm = TRUE) else NA,
      expected_range = meta[[country]][i]
    )
  }), fill = TRUE)
}

.phc_stats <- c("p25", "p50", "p75", "mean", "sd")

.phc_yearly_stats <- function(x) {
  q <- stats::quantile(x, c(0.25, 0.50, 0.75), names = FALSE)
  list(p25 = q[1], p50 = q[2], p75 = q[3], mean = mean(x), sd = stats::sd(x))
}

.phc_breaks <- function(dt, dataset, meta, break_vars, threshold) {
  if (!"year" %in% names(dt)) return(list(breaks = NULL, series = list()))

  vars <- meta[["MD_varname"]][tolower(meta[["class"]]) %in% "numeric"]
  if (!is.null(break_vars)) vars <- vars[vars %in% break_vars]
  vars <- vars[vars %in% names(dt)]

  flagged <- list()
  series <- list()
  for (v in vars) {
    x <- dt[[v]]
    # fread reads large whole numbers as bit64::integer64, which quantile(),
    # mean() and sd() keep as integer64; as double the statistics are right
    if (inherits(x, "integer64")) x <- as.double(x)
    keep <- !is.na(x)
    yearly <- data.table(year = dt[["year"]][keep], x = x[keep])
    yearly <- yearly[, .phc_yearly_stats(.SD[[1L]]), by = "year"]
    # no value in any year: nothing to compare (the NA check reports it)
    if (!nrow(yearly)) next
    setorderv(yearly, "year")

    growth <- rbindlist(lapply(.phc_stats, function(s) {
      prev <- shift(yearly[[s]])
      from_zero <- !is.na(prev) & prev == 0
      data.table(year = yearly[["year"]], stat = s,
                 growth = fifelse(from_zero, NA_real_, yearly[[s]] / prev - 1),
                 # a mean or sd leaving 0: the variable was all 0 (or constant) the year before
                 from_zero = from_zero & s %in% c("mean", "sd") & !is.na(yearly[[s]]) & yearly[[s]] != 0)
    }))
    # every variable's, for a page of a variable the NA check flags
    series[[v]] <- yearly
    g <- growth[["growth"]]
    # computed outside [ ]: inside, `growth` would be the column
    flag <- which((!is.na(g) & abs(g) > threshold) | growth[["from_zero"]])
    hit <- growth[flag]
    if (!nrow(hit)) next

    flagged[[v]] <- data.table(dataset = dataset, variable = v, stat = hit[["stat"]],
                               year = hit[["year"]], growth = hit[["growth"]],
                               from_zero = hit[["from_zero"]])
  }
  list(breaks = if (length(flagged)) rbindlist(flagged, use.names = TRUE), series = series)
}

# Share of NA values per year of each non-key variable; flagged where it
# changes by more than na_threshold from the year before, or is 1.
.phc_na_shares <- function(dt, dataset, meta, na_threshold) {
  if (!"year" %in% names(dt)) return(list(na_shares = NULL, series = list()))
  vars <- meta[["MD_varname"]][!meta[["is_key"]] %in% 1]
  vars <- vars[vars %in% names(dt) & vars != "year"]
  has_year <- !is.na(dt[["year"]])
  years <- dt[["year"]][has_year]

  flagged <- list()
  series <- list()
  for (v in vars) {
    shares <- data.table(year = years, na = is.na(dt[[v]][has_year]))
    shares <- shares[, list(na_share = mean(.SD[[1L]])), by = "year", .SDcols = "na"]
    setorderv(shares, "year")
    series[[v]] <- shares
    change <- shares[["na_share"]] - shift(shares[["na_share"]])
    all_na <- shares[["na_share"]] == 1
    hit <- which((!is.na(change) & abs(change) > na_threshold) | all_na)
    if (!length(hit)) next
    flagged[[v]] <- data.table(dataset = dataset, variable = v, year = shares[["year"]][hit],
                               na_share = shares[["na_share"]][hit], change = change[hit],
                               all_na = all_na[hit])
  }
  list(na_shares = if (length(flagged)) rbindlist(flagged, use.names = TRUE), series = series)
}

#' Plot the yearly share of NA values of a variable, marking flagged years
#'
#' @description
#' Plots the share of `NA` values of a variable per year, and labels the
#' years [mdi_check_harmonized()] flagged (a change above `na_threshold`, or
#' every value `NA`). Needs the ggplot2 package.
#'
#' @param na_series data.table, one variable's yearly share of `NA` values:
#'   `year`, `na_share` (an element of [mdi_check_harmonized()]'s
#'   `na_series`).
#' @param na_shares data.table, that variable's rows of
#'   [mdi_check_harmonized()]'s `na_shares` (`year`); may have no rows.
#' @param title character(1), the plot's title.
#'
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   s <- data.table::data.table(year = 2019:2021, na_share = c(0, 0, 0.3))
#'   mdi_plot_na_share(s, data.table::data.table(year = 2021), "SBS - emp")
#' }
#' @export
mdi_plot_na_share <- function(na_series, na_shares, title = "Share of NA values") {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("mdi_plot_na_share() needs the ggplot2 package.")
  check_dt(na_series, c("year", "na_share"), "na_series")
  check_dt(na_shares, "year", "na_shares")
  check_string(title, "title")
  # ggplot2's pronoun for columns of the plot data
  .data <- ggplot2::.data

  s <- data.table::copy(na_series)
  set(s, j = "year", value = as.integer(s[["year"]]))
  marked <- s[s[["year"]] %in% as.integer(na_shares[["year"]])]

  ggplot2::ggplot(s, ggplot2::aes(x = .data$year, y = .data$na_share)) +
    ggplot2::geom_line(linewidth = 1, color = "#7f7f7f") +
    ggplot2::geom_point(data = marked, size = 2, color = "#d62728") +
    ggplot2::geom_text(data = marked, ggplot2::aes(label = .data$year), vjust = -1, size = 3.5,
                       color = "#d62728") +
    ggplot2::scale_y_continuous(name = "Share of NA values", limits = c(0, 1.08),
                                breaks = seq(0, 1, 0.25), labels = function(x) paste0(x * 100, "%")) +
    ggplot2::scale_x_continuous(breaks = unique(s[["year"]])) +
    ggplot2::labs(title = title, x = "Year") +
    ggplot2::theme_minimal(base_size = 10)
}

#' Plot the yearly statistics of a variable, marking its breaks
#'
#' @description
#' Plots p25, p50, p75 and mean of a numeric variable per year on the left
#' axis and sd (dashed) on the right, and labels the years
#' [mdi_check_harmonized()] flagged as breaks. Needs the ggplot2 package.
#'
#' @param series data.table, one variable's yearly statistics: `year`, `p25`,
#'   `p50`, `p75`, `mean`, `sd` (an element of [mdi_check_harmonized()]'s
#'   `series`).
#' @param breaks data.table, that variable's rows of [mdi_check_harmonized()]'s
#'   `breaks` (`dataset`, `variable`, `stat`, `year`); may have no rows.
#' @param title character(1) or `NULL`: the plot's title. `NULL`:
#'   `paste("Break Detection in Aggregates:", dataset, "-", variable)`, from `breaks`.
#'
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   dt <- data.table::data.table(firmid = rep(letters[1:4], 3),
#'                                year = rep(2019:2021, each = 4),
#'                                emp = c(1:4, 1:4, 11:14))
#'   catalogue <- data.table::data.table(
#'     MD_dataset = "SBS", MD_varname = c("firmid", "year", "emp"),
#'     is_key = c(1L, 1L, 0L), class = c("character", "numeric", "numeric"),
#'     domain = "", FI = "2019 - 2021")
#'   res <- mdi_check_harmonized(dt, "SBS", catalogue, "FI")
#'   mdi_plot_breaks(res$series$emp, res$breaks)
#' }
#' @export
mdi_plot_breaks <- function(series, breaks, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("mdi_plot_breaks() needs the ggplot2 package.")
  check_dt(series, c("year", .phc_stats), "series")
  check_dt(breaks, c("dataset", "variable", "stat", "year"), "breaks")
  if (is.null(title)) {
    title <- paste("Break Detection in Aggregates:", breaks[["dataset"]][1], "-", breaks[["variable"]][1])
  }
  check_string(title, "title")
  # ggplot2's pronoun for columns of the plot data
  .data <- ggplot2::.data

  long <- melt(series, id.vars = "year", measure.vars = .phc_stats,
               variable.name = "stat", value.name = "value", variable.factor = FALSE)
  marked <- merge(long, unique(breaks[, c("year", "stat")]), by = c("year", "stat"))
  set(long, j = "year", value = as.integer(long[["year"]]))
  set(marked, j = "year", value = as.integer(marked[["year"]]))

  # sd on its own scale, drawn against a secondary axis
  is_sd <- long[["stat"]] == "sd"
  scaling_factor <- max(long[["value"]][!is_sd], na.rm = TRUE) / max(long[["value"]][is_sd], na.rm = TRUE)
  # sd NA (one value a year) or 0: drawn on the main scale
  if (!is.finite(scaling_factor) || scaling_factor == 0) scaling_factor <- 1
  rescale <- function(d) set(d, j = "value_scaled",
                             value = ifelse(d[["stat"]] == "sd", d[["value"]] * scaling_factor, d[["value"]]))
  rescale(long)
  rescale(marked)
  main <- long[long[["stat"]] != "sd"]
  sd_line <- long[long[["stat"]] == "sd"]
  marked_main <- marked[marked[["stat"]] != "sd"]
  marked_sd <- marked[marked[["stat"]] == "sd"]
  sd_colour <- "#9467bd"

  ggplot2::ggplot(long, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_line(data = main, ggplot2::aes(y = .data$value_scaled, color = .data$stat,
                                                 group = .data$stat), linewidth = 1) +
    # sd is mapped to linetype only, so it stays out of the colour legend
    ggplot2::geom_line(data = sd_line, ggplot2::aes(y = .data$value_scaled, linetype = "sd",
                                                    group = .data$stat),
                       color = sd_colour, linewidth = 1) +
    ggplot2::geom_point(data = marked_main, ggplot2::aes(y = .data$value_scaled, color = .data$stat),
                        size = 2) +
    ggplot2::geom_text(data = marked_main, ggplot2::aes(y = .data$value_scaled, label = .data$year,
                                                        color = .data$stat),
                       vjust = -1, size = 3.5, show.legend = FALSE) +
    ggplot2::geom_point(data = marked_sd, ggplot2::aes(y = .data$value_scaled), color = sd_colour,
                        size = 2) +
    ggplot2::geom_text(data = marked_sd, ggplot2::aes(y = .data$value_scaled, label = .data$year),
                       vjust = -1, size = 3.5, color = sd_colour, show.legend = FALSE) +
    ggplot2::scale_y_continuous(name = "Main stats (p25/p50/p75/mean)",
                                sec.axis = ggplot2::sec_axis(~ . / scaling_factor,
                                                             name = "Standard deviation (sd)")) +
    ggplot2::scale_x_continuous(breaks = unique(long[["year"]])) +
    ggplot2::scale_color_manual(values = c(p25 = "#1f77b4", p50 = "#ff7f0e",
                                           p75 = "#2ca02c", mean = "#d62728"),
                                breaks = c("p25", "p50", "p75", "mean")) +
    ggplot2::scale_linetype_manual(values = c(sd = "dashed"),
                                   guide = ggplot2::guide_legend(override.aes = list(color = sd_colour))) +
    ggplot2::labs(title = title,
                  x = "Year", y = NULL, color = "Statistic", linetype = NULL) +
    ggplot2::theme_minimal(base_size = 10)
}
