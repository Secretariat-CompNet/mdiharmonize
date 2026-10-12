#' Read one NSI data file, ready to harmonize
#'
#' @description
#' Reads one raw NSI data file with [mdi_import_data()], only the columns the
#' concordance needs for that file's year (plus its year variable and the
#' columns its preprocessing needs), then runs its preprocessing, if any
#' ([mdi_preprocess()]).
#'
#' Columns are read as text where a number-like reading would lose
#' information: variables that are recoded/reclassed, NACE/PRODCOM/CN8
#' codes, variables the NSI declares `character` or `date` (a `ddmmyyyy` date
#' or a zero-padded code read as a number loses its leading zero), and the
#' column a `reshape` preprocessing step takes its names from.
#'
#' @param cfg config list from [mdi_config()], with the NSI metadata loaded
#'   and selected (`cfg$nsi` from [mdi_load_nsi_metadata()] and
#'   [mdi_select_md_names()]).
#' @param file_row data.table, the file's row of `cfg$nsi$NSI_datafiles`.
#' @param oracle_channel an open RODBC connection, for `format == "oracle"`.
#'
#' @return The file as a data.table.
#' @keywords internal
mdi_read_nsi_datafile <- function(cfg, file_row, oracle_channel = NULL) {
  check_cfg(cfg, need = "nsi")
  check_dt(file_row, c("NSI_dataset", "NSI_datafile", "yearvar", "year_start", "format", "path"),
           "file_row")
  if (nrow(file_row) != 1L) stop("'file_row' must be one row of NSI_datafiles.")

  spec <- .import_spec(file_row, cfg$nsi$NSI_MD_conc, cfg$nsi$NSI_varnames)
  dt <- .import_nsi_file(cfg, file_row, spec$cols, spec$char_cols, oracle_channel)

  preprocessing <- file_row[["preprocessing"]]
  if (.is_set(preprocessing)) {
    # a mergefrom step reads its other file as a file of its own
    read_datafile <- function(datafile, cols, text_cols) {
      .read_merge_source(cfg, datafile, cols, text_cols, oracle_channel)
    }
    dt <- mdi_preprocess(preprocessing, dt, read_datafile = read_datafile)
  }
  dt
}

# Import the columns `cols` of one NSI data file (its row of NSI_datafiles),
# `char_cols` as text.
.import_nsi_file <- function(cfg, file_row, cols, char_cols, oracle_channel) {
  file_name <- file_row[["NSI_datafile"]]
  file_format <- file_row[["format"]]
  is_oracle <- tolower(file_format) == "oracle"
  if (!is_oracle) {
    path <- paste0(cfg$dirs$inputdata, file_row[["path"]], file_name, ".", file_format)
    if (!file.exists(path)) {
      stop("NSI data file not found: ", path, ". Its path is dirINPUTDATA + the 'path' column of ",
           cfg$country, "_datafiles.csv. For mock data with one folder per country, dirINPUTDATA ",
           "must include the country, e.g. paste0(<mock data folder>, CountryCode, \"/\") ",
           "(see launchpad/countdown.R).", call. = FALSE)
    }
  }
  mdi_import_data(
    # for Oracle, path is the DSN
    dir = if (is_oracle) file_row[["path"]] else paste0(cfg$dirs$inputdata, file_row[["path"]]),
    file = if (is_oracle) file_name else paste0(file_name, ".", file_format),
    format = file_format,
    col_list = cols,
    char_columns = char_cols,
    encoding = file_row[["encoding"]],
    oracle_channel = oracle_channel,
    db_user = cfg$db_user
  )
}

# The other file of a mergefrom step, read with its own NSI_datafiles row,
# the columns it declares character or date and `text_cols` as text.
.read_merge_source <- function(cfg, datafile, cols, text_cols, oracle_channel) {
  datafiles <- cfg$nsi$NSI_datafiles
  file_row <- datafiles[datafiles[["NSI_datafile"]] == datafile]
  if (nrow(file_row) != 1L) {
    stop("mergefrom: '", datafile, "' must be exactly one NSI_datafile of ", cfg$country,
         "_datafiles.csv, found ", nrow(file_row), ".", call. = FALSE)
  }
  varnames <- cfg$nsi$NSI_varnames
  declared <- varnames[["NSI_varname"]][which(varnames[["NSI_datafile"]] == datafile &
                                                tolower(varnames[["class"]]) %in% c("character", "date"))]
  text <- unique(c(text_cols, intersect(declared, cols)))
  .import_nsi_file(cfg, file_row, cols, if (length(text)) text, oracle_channel)
}

# Which columns of one NSI data file to read, and which of those as text.
.import_spec <- function(file_row, conc, nsi_varnames) {
  nsi <- file_row[["NSI_dataset"]]
  file_name <- file_row[["NSI_datafile"]]
  year_var <- file_row[["yearvar"]]

  # which(): NA in a condition counts as FALSE, as in a data.table filter
  in_nsi <- conc[["NSI_dataset"]] == nsi
  required <- unique(conc[["NSI_varname"]][which(in_nsi & conc[["year"]] == file_row[["year_start"]])])
  if (.is_set(year_var)) required <- unique(c(required, year_var))

  # only the file's own variables, matched by prefix; plus the ones its
  # preprocessing needs, which it may create itself
  in_file <- nsi_varnames[["NSI_datafile"]] == file_name
  native <- nsi_varnames[["NSI_varname"]][which(in_file)]
  cols <- native[vapply(native, function(v) any(startsWith(required, v)), logical(1))]
  cols <- unique(c(cols, unlist(file_row[["prepro_vars"]])))

  # read as text: recoded/reclassed variables, classification and region
  # codes (which can have leading zeros), and what the NSI declares character
  # or date
  codes <- c("nace", "pcc8", "cn08", "nuts1", "nuts2", "nuts3", "lau")
  text <- unique(conc[["NSI_varname"]][which(in_nsi & grepl("recode|reclass", conc[["method"]]))])
  text <- c(text, unique(conc[["NSI_varname"]][conc[["MD_varname"]] %in% codes]))
  text <- text[text %in% cols]
  declared_text <- nsi_varnames[["NSI_varname"]][which(in_file & tolower(nsi_varnames[["class"]]) %in% c("character", "date"))]
  text <- unique(c(text, declared_text[declared_text %in% cols]))

  # a reshape's names_from values become column names
  steps <- .parse_preprocessing(file_row[["preprocessing"]])
  reshape <- Filter(function(step) step$op == "reshape", steps)
  if (length(reshape)) text <- c(text, reshape[[1]]$args$names_from_vars)

  text <- unlist(text)
  list(cols = cols, char_cols = if (length(text)) text)
}
