#' Internal global declarations
#'
#' Declares global variables to satisfy R CMD check when using non-standard
#' evaluation or generated column names.
#'
#' @name globals
#' @keywords internal
#' @importFrom stats uniroot pgamma predict rgamma density quantile coef setNames
#' @importFrom utils head
#' @importFrom nimble getNimbleOption nimbleInternalFunctions nimSwitch messageIfVerbose
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
  "stick_breaking"
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
