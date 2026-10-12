# Validation for the eval(parse(text = ...)) call sites that run R code from
# a country's metadata: mdi_harmonize()'s revalue/redefine (the concordance's
# `detail`) and mdi_preprocess()'s derive (NSI_datafiles.csv's
# `preprocessing`). Unchecked, that text could call anything (system(), file
# I/O, ...). mdi_check_safe_expr() checks every function the parsed
# expression calls against the lists below before the caller evaluates it.
# It only validates: evaluating in a sandbox environment broke data.table's
# column-name lookup inside DT[i, j]. case_when()/recode()/str_remove_all()
# resolve through the package imports (dplyr, stringr).
#
# The lists come from the functions the countries' concordance and
# preprocessing expressions use, plus common side-effect-free math/string
# functions. A function an expression
# needs that isn't listed can be added after confirming it only computes on
# its arguments (no I/O, no environment access).

# Base R/data.table syntax every expression needs regardless of what it
# computes -- operators, grouping, assignment, dplyr's formula syntax -- as
# opposed to the "business" functions in .mdi_safe_fns below.
.mdi_safe_syntax <- c(
  "(", "{", "[", "[[", "$", ":", "~",
  "+", "-", "*", "/", "^", "%%", "%/%", "%in%",
  "==", "!=", "<", "<=", ">", ">=", "&", "&&", "|", "||", "!",
  "<-", "=", ":=", "function"
)

# Business functions: (1) used in the countries' `detail`/`preprocessing`
# values; (2) a buffer of other common, side-effect-free
# math/string/logical functions, not yet used but plausible and safe.
.mdi_safe_fns <- c(
  # (1) surveyed -- currently or historically used
  "as.Date", "as.character", "as.integer", "as.logical", "as.numeric",
  "c", "case_when", "factor", "fcase", "fifelse", "format", "grepl",
  "gsub", "ifelse", "is.na", "lapply", "log", "mapply", "nchar", "nzchar",
  "paste", "paste0", "pmax", "recode", "Reduce", "round", "rowSums",
  "sprintf", "str_remove_all", "sub", "substr", "substring", "sum",
  "trimws", "unique",
  # (2) common safe buffer -- pure math/string/logical, no I/O, not yet
  # seen in real data but low-risk to allow proactively
  "abs", "all", "any", "ceiling", "endsWith", "exp", "floor", "length",
  "max", "mean", "median", "min", "rev", "sd", "sort", "sqrt",
  "startsWith", "strsplit", "tolower", "toupper", "which"
)

# Rejects `expr_str` if it calls any function outside the combined
# syntax + whitelist. Walks the parsed call tree looking only at call heads
# (e.g. `log` in `log(x)`) -- bare symbols like column names or `DT` are
# never restricted, only what gets *invoked*.
mdi_check_safe_expr <- function(expr_str) {
  check_string(expr_str, "expr_str")

  parsed <- tryCatch(parse(text = expr_str), error = function(e) {
    stop("mdi_check_safe_expr(): could not parse expression '", expr_str,
         "': ", conditionMessage(e))
  })

  allowed <- c(.mdi_safe_syntax, .mdi_safe_fns)
  # a function handed to another one runs too: lapply(x, system) or
  # Reduce("system", ...) must be refused like system() itself
  check_fun_arg <- function(e, fn_name) {
    pos <- if (fn_name %in% c("lapply", "sapply", "vapply")) 2L else 1L
    args <- as.list(e)[-1]
    nm <- names(args)
    fun_arg <- if (!is.null(nm) && any(nm %in% c("FUN", "f"))) args[[which(nm %in% c("FUN", "f"))[1]]]
               else if (length(args) >= pos) args[[pos]]
    if (is.null(fun_arg) || is.call(fun_arg)) return(invisible())
    fun_name <- if (is.name(fun_arg)) as.character(fun_arg) else if (is.character(fun_arg)) fun_arg[1]
    if (!is.null(fun_name) && !fun_name %in% allowed) refuse(fun_name)
  }
  refuse <- function(fn_name) {
    stop("mdi_check_safe_expr(): '", fn_name, "' is not on the allowed function list for ",
         "concordance and preprocessing expressions: only functions without side effects ",
         "(no file, system, network or environment access) may run there. If it is a ",
         "legitimate transformation, add it to .mdi_safe_fns in mdi_check_safe_expr.R. ",
         "Expression: '", expr_str, "'.", call. = FALSE)
  }

  # R represents an omitted call argument (e.g. `DT[, x := 1]`'s empty `i`
  # slot) with a special sentinel that errors the moment it's *used* in any
  # way -- even is.call()/identical() on it -- though extracting it (e[[i]])
  # is fine on its own. Recursing into it via walk() forces that use inside
  # walk()'s own body (is.call(e)); catch exactly that known R error message
  # and treat it as "not a function call, skip" -- anything else (including
  # this function's own "not in the allowed list" stop()) re-throws.
  walk <- function(e) {
    if (is.call(e)) {
      fn_name <- as.character(e[[1]])[1]
      if (!(fn_name %in% allowed)) refuse(fn_name)
      if (fn_name %in% c("lapply", "sapply", "vapply", "mapply", "Map", "Reduce", "Filter")) {
        check_fun_arg(e, fn_name)
      }
      n <- length(e)
      if (n > 1) {
        for (i in 2:n) {
          tryCatch(
            walk(e[[i]]),
            error = function(err) {
              if (grepl("missing, with no default", conditionMessage(err), fixed = TRUE)) {
                invisible(NULL)
              } else {
                stop(err)
              }
            }
          )
        }
      }
    }
  }

  for (e in parsed) walk(e)
  invisible(TRUE)
}
