# Kernel registry --------------------------------------------------------------

.kernel_registry <- new.env(parent = emptyenv())

init_kernel_registry <- function() {

  # Gamma: shape (no covariate link by default), scale > 0 with log link
  .kernel_registry$gamma <- list(
    name   = "gamma",
    params = c("shape", "scale"),
    support = c(lower = 0, upper = Inf),
    default_trans = list(
      shape = NULL,        # no covariate link by default
      scale = "exp"        # scale(x) = exp(xb)
    )
  )

  # Lognormal: meanlog real, sdlog > 0
  .kernel_registry$lognormal <- list(
    name   = "lognormal",
    params = c("meanlog", "sdlog"),
    support = c(lower = 0, upper = Inf),
    default_trans = list(
      meanlog = "identity",  # meanlog(x) = xb
      sdlog   = "exp"        # sdlog(x) = exp(xb)
    )
  )

  # Normal: mean real, sd > 0
  .kernel_registry$normal <- list(
    name   = "normal",
    params = c("mean", "sd"),
    support = c(lower = -Inf, upper = Inf),
    default_trans = list(
      mean = "identity",
      sd   = "exp"
    )
  )

  # Laplace: location real, scale > 0
  .kernel_registry$laplace <- list(
    name   = "laplace",
    params = c("location", "scale"),
    support = c(lower = -Inf, upper = Inf),
    default_trans = list(
      location = "identity",
      scale    = "exp"
    )
  )

  # Inverse Gaussian: mean > 0, shape > 0 (shape not regressed by default)
  .kernel_registry$inverse_gaussian <- list(
    name   = "inverse_gaussian",
    params = c("mean", "shape"),
    support = c(lower = 0, upper = Inf),
    default_trans = list(
      mean  = "exp",
      shape = NULL
    )
  )

  # Amoroso: location, scale, shape1, shape2
  .kernel_registry$amoroso <- list(
    name   = "amoroso",
    params = c("location", "scale", "shape1", "shape2"),
    support = c(lower = 0, upper = Inf),
    default_trans = list(
      location = "identity",
      scale    = "exp",
      shape1   = NULL,
      shape2   = NULL
    )
  )

  # Pareto: scale (xm) > 0, shape > 0
  .kernel_registry$pareto <- list(
    name   = "pareto",
    params = c("scale", "shape"),
    support = c(lower = 0, upper = Inf),
    default_trans = list(
      scale = "exp",   # positive
      shape = NULL     # no covariates by default
    )
  )
}

#' Get a registered kernel
#' @param kernel Character name of the kernel.
#' @return A list describing the kernel.
#' @export
get_kernel <- function(kernel) {
  if (!is.character(kernel) || length(kernel) != 1L) {
    stop("`kernel` must be a single character string.")
  }
  k <- .kernel_registry[[kernel]]
  if (is.null(k)) {
    stop("Unknown kernel '", kernel,
         "'. Use register_kernel() to add it first.")
  }
  k
}

#' Register a custom kernel
#'
#' @param name Character name of the kernel.
#' @param params Character vector of parameter names.
#' @param support Numeric vector of length 2: lower and upper support.
#' @param default_trans Named list or character vector of default transformations
#'   mapping linear predictor 'xb' to each parameter. Each element should be
#'   either NULL (no covariate link), a single character label such as
#'   "identity", "exp", "log", "inv", "sq", or "sqrt", or a function that
#'   takes xb and returns the parameter value.
#' @export
register_kernel <- function(name, params, support, default_trans) {
  stopifnot(is.character(name), length(name) == 1L)
  stopifnot(is.character(params), length(params) >= 1L)
  stopifnot(is.numeric(support), length(support) == 2L)
  if (is.character(default_trans)) {
    default_trans <- as.list(default_trans)
  }
  if (!all(names(default_trans) %in% params)) {
    stop("All names of `default_trans` must be in `params`.")
  }
  .kernel_registry[[name]] <- list(
    name   = name,
    params = params,
    support = c(lower = support[1L], upper = support[2L]),
    default_trans = default_trans
  )
  invisible(name)
}
