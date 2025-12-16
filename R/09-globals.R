#' Declare global variables for R CMD check
#'
#' Avoid NOTES about "no visible binding for global variable" in NSE code
#' used by ggplot2.
#'
#' @keywords internal
#' @noRd
utils::globalVariables(c(
  ".iter", "value", "x", "y", "ehat",
  "term", "lower", "upper",
  "tau", "mean", "q1", "q2", "q3", "row"
))
