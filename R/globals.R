#' Internal global declarations
#'
#' Declares global variables to satisfy R CMD check when using non-standard
#' evaluation or generated column names.
#'
#' @name globals
#' @keywords internal
#' @importFrom stats uniroot pgamma predict rgamma density quantile coef setNames
<<<<<<< HEAD
#' @importFrom stats fitted residuals kernel plogis qlogis sd
#' @importFrom rlang .data
=======
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
#' @importFrom utils head
#' @import nimble
NULL

utils::globalVariables(c(
  ".kernel_registry_env",
  ".kernel_registry_ready",
  "ddirch",
  "dcat",
  "dbeta",
  "inprod",
  "v",
  "pow",
  "ddexp",
  "pdexp",
  "rdexp",
  "components",
  "BULK_DECL_PLACEHOLDER",
  "CONC_PLACEHOLDER",
  "GPD_BLOCK",
  "HASX_BETA_BLOCK",
  "HASX_DET_BLOCK",
  "LIKELIHOOD_BLOCK",
  "logit<-",
  "S",
  "S_lower",
  "S_upper",
  "arm",
<<<<<<< HEAD
  "stick_breaking",
  ".data",
  "conc_plan",
  "estimate",
  "fitted",
  "group",
  "id",
  "index",
  "kernel",
  "lower",
  "observed",
  "plogis",
  "probit<-",
  "ps",
  "qlogis",
  "residuals",
  "sd",
  "survival",
  "type",
  "upper",
  "value",
  "x",
  "y"
=======
  "stick_breaking"
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
))
#' Package hooks
#'
#' Internal package initialization.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  init_kernel_registry()
  invisible()
}
