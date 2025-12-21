#' Internal global declarations
#'
#' Declares global variables to satisfy R CMD check when using non-standard
#' evaluation or generated column names.
#'
#' @name globals
#' @keywords internal
#' @importFrom  stats uniroot pgamma predict rgamma
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
  "Param1",
  "Param2",
  "beta_Param1",
  "beta_Param2",
  "pow",
  "ddexp",
  "pdexp",
  "rdexp"
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
