#+ filtered ###################################################################
#' Global contracts for DPmixGPD
#'
#' These lists and helpers capture the frozen modeling rules that must hold
#' everywhere in the package: backends, kernels, GPD usage, and mixture sizes.
#' They are intentionally loaded before the rest of the core code so downstream
#' builders can reuse the same constants.
#'
#' @keywords internal
#' @noRd
allowed_backends <- c("crp", "sb")
allowed_kernels <- c("gamma", "lognormal", "invgauss", "normal", "laplace", "cauchy", "amoroso")
positive_support_kernels <- c("gamma", "lognormal", "invgauss", "amoroso")
real_support_kernels <- c("normal", "laplace", "cauchy")

is_allowed_kernel <- function(x) {
  as.character(x) %in% allowed_kernels
}

check_gpd_contract <- function(GPD, kernel) {
  if (isTRUE(GPD) && identical(as.character(kernel), "cauchy")) {
    stop("Cauchy kernels are never paired with GPD tails.", call. = FALSE)
  }
  invisible(NULL)
}
