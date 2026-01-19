#' Internal global declarations
#'
#' Declares global variables to satisfy R CMD check when using non-standard
#' evaluation or generated column names.
#'
#' @name globals
#' @keywords internal
#' @importFrom stats uniroot pgamma predict rgamma density quantile coef setNames
#' @importFrom stats fitted residuals kernel plogis qlogis sd
#' @importFrom rlang .data
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
