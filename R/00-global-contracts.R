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
is_allowed_backend <- function(x) {
  as.character(x) %in% allowed_backends
}

is_allowed_kernel <- function(x) {
  as.character(x) %in% allowed_kernels
}

kernel_support_type <- function(kernel) {
  kernel <- as.character(kernel)
  if (kernel %in% positive_support_kernels) return("positive")
  if (kernel %in% real_support_kernels) return("real")
  "unknown"
}

check_gpd_contract <- function(GPD, kernel) {
  if (isTRUE(GPD) && identical(as.character(kernel), "cauchy")) {
    stop("Cauchy kernels are never paired with GPD tails.", call. = FALSE)
  }
  invisible(NULL)
}

check_mixture_components <- function(components) {
  if (components < 2L || components > 4L) {
    stop("Mixture truncation components must be between 2 and 4 inclusive.", call. = FALSE)
  }
  invisible(NULL)
}
