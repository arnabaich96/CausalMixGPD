#' Kernel registry and capability checks
#'
#' The package separates model-fitting code from distribution-specific code by
#' using a registry of kernel definitions. A kernel definition records (i) the
#' kernel name, (ii) whether it is supported by each back end (CRP / SB) and by
#' the GPD tail extension, and (iii) pointers to the relevant R functions (for
#' example, \code{rGammaMix} or \code{rGammaMixGPD}) that live in the
#' distribution files.
#'
#' The model engine assumes a canonical two-parameter interface (\code{Param1},
#' \code{Param2}). Kernel-specific parameter naming is handled only for
#' presentation in \code{summary()} outputs.
#'
#' @name kernel_registry
#' @keywords internal
NULL

.kernel_registry_env <- new.env(parent = emptyenv())
.kernel_registry_ready <- FALSE

#' Initialize kernel registry
#'
#' This function populates the internal registry with the kernels used by the
#' package. It is called from \code{.onLoad()} and may be called manually in
#' interactive sessions.
#'
#' @return Invisibly returns \code{TRUE}.
#' @keywords internal
init_kernel_registry <- function() {
  if (isTRUE(.kernel_registry_ready)) return(invisible(TRUE))

  # Helper to safely look up a function in the package namespace.
  .get_fun <- function(nm) {
    get0(nm, envir = asNamespace("DPmixGPD"), inherits = FALSE)
  }

  .kernel_registry_env$gamma <- list(
    name = "gamma",
    n_params = 2L,
    supports_tail = TRUE,
    # mixture-level RNGs (draws x obs handled by wrappers)
    rMix = .get_fun("rGammaMix"),
    rMixGPD = .get_fun("rGammaMixGPD")
  )

  .kernel_registry_env$normal <- list(
    name = "normal",
    n_params = 2L,
    supports_tail = TRUE,
    rMix = .get_fun("rNormMix"),
    rMixGPD = .get_fun("rNormMixGPD")
  )

  .kernel_registry_env$lognormal <- list(
    name = "lognormal",
    n_params = 2L,
    supports_tail = TRUE,
    rMix = .get_fun("rLognormalMix"),
    rMixGPD = .get_fun("rLognormalMixGPD")
  )

  .kernel_registry_env$laplace <- list(
    name = "laplace",
    n_params = 2L,
    supports_tail = TRUE,
    rMix = .get_fun("rLaplaceMix"),
    rMixGPD = .get_fun("rLaplaceMixGPD")
  )

  .kernel_registry_env$inverse_gaussian <- list(
    name = "inverse_gaussian",
    n_params = 2L,
    supports_tail = TRUE,
    rMix = .get_fun("rInvGaussMix"),
    rMixGPD = .get_fun("rInvGaussMixGPD")
  )

  .kernel_registry_env$cauchy <- list(
    name = "cauchy",
    n_params = 2L,
    supports_tail = FALSE,
    rMix = .get_fun("rCauchyMix"),
    rMixGPD = NULL
  )

  .kernel_registry_env$pareto <- list(
    name = "pareto",
    n_params = 2L,
    supports_tail = FALSE,
    rMix = .get_fun("rParetoMix"),
    rMixGPD = NULL
  )

  # Amoroso exists as a distribution file but is not part of the Param1/Param2
  # unified interface because it has four parameters. It is therefore excluded
  # from this registry for model fitting.
  .kernel_registry_ready <- TRUE
  invisible(TRUE)
}

#' Get a kernel definition
#'
#' @param kernel Character scalar naming a registered kernel.
#' @return A kernel definition list.
#' @export
get_kernel_def <- function(kernel) {
  if (!is.character(kernel) || length(kernel) != 1L) {
    stop("`kernel` must be a single character string.", call. = FALSE)
  }
  init_kernel_registry()
  k <- .kernel_registry_env[[kernel]]
  if (is.null(k)) {
    stop("Unknown kernel '", kernel, "'.", call. = FALSE)
  }
  k
}

#' Check kernel support for a model configuration
#'
#' This check is intentionally strict. If a kernel does not provide the
#' required mixture RNG function(s), the fit is rejected early with a clear
#' error message rather than failing later inside MCMC or prediction code.
#'
#' @param kernel Kernel name.
#' @param tail Logical. Whether the GPD extension is requested.
#' @return Invisibly returns \code{TRUE}.
#' @keywords internal
.check_kernel_support <- function(kernel, tail = FALSE) {
  k <- get_kernel_def(kernel)

  if (!isTRUE(k$n_params == 2L)) {
    stop("Kernel '", kernel, "' is not a two-parameter kernel in this setup.",
         call. = FALSE)
  }

  if (is.null(k$rMix) || !is.function(k$rMix)) {
    stop("Kernel '", kernel, "' is missing required function rMix.", call. = FALSE)
  }

  if (isTRUE(tail)) {
    if (!isTRUE(k$supports_tail) || is.null(k$rMixGPD) || !is.function(k$rMixGPD)) {
      stop("Kernel '", kernel, "' is not available for the GPD tail extension.",
           call. = FALSE)
    }
  }

  invisible(TRUE)
}
