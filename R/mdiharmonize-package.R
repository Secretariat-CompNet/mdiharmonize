#' @keywords internal
#'
#' @section Acknowledgements:
#' We thank Jorge de la Cal Medina, Ashim Dubey, Dominik Frommeyer (Statistisches Bundesamt, Destatis), Adam Haunch and Rachel Kühn (Statistisches Bundesamt, Destatis).
#'
#' Funded by the European Union under the Technical Support Instrument (TSI). This package was developed as part of the project ProdTool (TSI-2025-MULTIMSPROD-IBA, TSI Project Number: 101261159). Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or the European Commission. Neither the European Union nor the granting authority can be held responsible for them.
#'
"_PACKAGE"

# Packages the package uses unqualified
#' @import data.table
#' @import mditools
#' @importFrom stats as.formula binomial coef complete.cases confint fitted glm
#'   lm model.frame na.exclude na.omit optim pchisq pnorm poly predict
#'   quantile runif setNames weighted.mean
#' @importFrom utils head tail write.table
# the non-base functions concordance and preprocessing expressions may call
# (mdi_check_safe_expr()'s whitelist), so they work with neither attached
#' @importFrom dplyr case_when recode
#' @importFrom stringr str_remove_all
NULL
