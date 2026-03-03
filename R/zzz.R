#' Internal global declarations
#'
#' Declares global variables to satisfy R CMD check when using non-standard
#' evaluation or generated column names.
#'
#' @name globals
#' @keywords internal
#' @importFrom stats uniroot pgamma predict rgamma density quantile coef setNames fitted plogis qlogis residuals sd
#' @importFrom utils head
#' @import nimble ggplot2
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
  "probit<-",
  "S",
  "S_lower",
  "S_upper",
  "arm",
  "cluster",
  "count",
  "dataset",
  "max_prob",
  "stick_breaking",
  # ggplot2 NSE variables
  "Chain",
  "Parameter",
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
  "ps",
  "qlogis",
  "residuals",
  "sd",
  "survival",
  "type",
  "upper",
  "value",
  "x",
  "y",
  "z"
))
#' Package hooks
#'
#' Internal package initialization.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Initialize kernel registry if available
  # (may not be loaded during roxygen documentation generation)
  init_kernel_registry_fun <- get0(
    "init_kernel_registry",
    mode = "function",
    inherits = TRUE
  )
  if (!is.null(init_kernel_registry_fun)) {
    init_kernel_registry_fun()
  }
  if (is.null(getOption("CausalMixGPD.plotly"))) {
    options(CausalMixGPD.plotly = FALSE)
  }
  # Wrap exported functions if utility is available
  wrap_exported_silent_fun <- get0(
    ".wrap_exported_silent",
    mode = "function",
    inherits = TRUE
  )
  if (!is.null(wrap_exported_silent_fun)) {
    wrap_exported_silent_fun(pkgname, opt_name = "CausalMixGPD.silent")
  }
  invisible()
}
