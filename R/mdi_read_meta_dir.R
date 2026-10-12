#' Read a directory of MD/NSI metadata CSVs into a named list of data.tables
#'
#' @description
#' The shared "read every metadata file in a directory, the same way" loop
#' behind `mdi_load_md_metadata()` and `mdi_load_nsi_metadata()`. Every
#' column is read as character (at read time, so codes keep their leading
#' zeros, e.g. NACE `"0111"`) except those named in `char_except`
#' (defaults to `year`/`is_key` — the two known non-code columns in the
#' MD/NSI metadata family: `year` is a real date, `is_key` a boolean flag).
#' These files are classification/lookup tables where every other column is
#' a code, and a numeric-looking code (e.g. `MD_codebook.csv`'s `code`
#' column) read as a number loses its leading zeros. If present, `year` is
#' additionally coerced to numeric.
#'
#' Not for reading `auxdata/deflators.csv`-style numeric panel data — those
#' files are mostly measurement values with a couple of ID columns, the
#' opposite default (`mdi_load_deflators()` keeps its own logic).
#'
#' @param dir Character, the directory to scan.
#' @param pattern Character, a regular expression matched against file names
#'   within `dir` (non-recursive) — e.g. `"_hier\\.csv$"` matches every
#'   `..._hier.csv` file in `dir`, not just one.
#' @param char_except Character vector of column names to leave as read
#'   (not forced to character) wherever they appear.
#'
#' @return A named list of `data.table`s, one per matched file, named after
#'   the file's own basename without extension (e.g. `MD_nace_hier.csv` ->
#'   `MD_nace_hier`).
#'
#' @keywords internal
mdi_read_meta_dir <- function(dir, pattern, char_except = c("year", "is_key")) {
  check_string(dir, "dir")
  check_string(pattern, "pattern")

  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  result <- lapply(files, function(f) {
    # character at read time, not converted afterwards: once fread has parsed
    # a code like "0111" as a number, the leading zero is already gone
    char_cols <- setdiff(names(data.table::fread(f, nrows = 0)), char_except)
    col_classes <- if (length(char_cols) > 0) list(character = char_cols) else NULL
    dt <- data.table::fread(f, fill = TRUE, na.strings = "", colClasses = col_classes)
    if ("year" %in% names(dt)) {
      data.table::set(dt, j = "year", value = as.numeric(dt[["year"]]))
    }
    dt
  })
  names(result) <- tools::file_path_sans_ext(basename(files))
  result
}

# Reads one file plainly (no character coercion), or NULL if it doesn't
# exist. For the one metadata file per domain (MD_datafiles.csv /
# NSI_datafiles.csv) that has genuine numeric columns (year_start/year_end)
# the mdi_read_meta_dir() rule would wrongly stringify — shared by
# mdi_load_md_metadata() and mdi_load_nsi_metadata() so the exception lives
# in one place, not two copy-pasted fread() calls.
# "NA" also counts as missing here (unlike in the code tables, where "NA" can
# be a real code): these are config files, and e.g. DE_datafiles.csv writes
# "NA" in yearvar to mean "no year variable".
mdi_read_plain_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  data.table::fread(path, fill = TRUE, na.strings = c("", "NA"))
}
