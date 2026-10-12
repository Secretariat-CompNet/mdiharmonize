#' Write a message to a log file
#'
#' @description
#' Writes (or appends) a message to a text file, for warnings, diagnostics and
#' other information a run should keep, instead of printing to the console.
#' `WarningsDescription.txt` in the output folder is the run's default log;
#' the checkers and the post-harmonization report write their own files.
#'
#' @param message character, the message; a vector is written one element per
#'   line.
#' @param path character(1), the folder the file is in, ending in `/` (e.g.
#'   `cfg$dirs$output`).
#' @param append logical(1), add to the file (`TRUE`) or start it anew.
#' @param file_name character(1), the file's name.
#'
#' @return `NULL`, invisibly.
#' @examples
#' dir <- paste0(tempfile("log_"), "/")
#' dir.create(dir)
#' log_warnings("first line", dir, append = FALSE)
#' log_warnings("second line", dir)
#' readLines(paste0(dir, "WarningsDescription.txt"))
#' @export
log_warnings <- function(message, path, append = TRUE, file_name = "WarningsDescription.txt") {
  if (missing(path)) stop("log_warnings(): 'path' is required, the folder the log file is in.")
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop("log_warnings(): 'path' must be a non-empty character string.")
  write(message, file = paste0(path, file_name), append = append)
}
