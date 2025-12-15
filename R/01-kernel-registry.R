# Kernel registry --------------------------------------------------------------

.kernel_registry <- new.env(parent = emptyenv())
#' Init kernel registry
#'
#' Init kernel registry.
#'
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("init_kernel_registry", "DPmixGPD")
#' f
#'
#' @keywords internal
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
#' Get kernel
#'
#' Get kernel.
#'
#' @param kernel kernel.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("get_kernel", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
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
#' Register kernel
#'
#' Register kernel.
#'
#' @param name name.
#' @param params params.
#' @param support support.
#' @param default_trans default_trans.
#'
#' @return See details.
#'
#' @examples
#' f <- getFromNamespace("register_kernel", "DPmixGPD")
#' f
#' \dontrun{
#' # Example usage (may require MCMC run and nimble compilation).
#' }
#'
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


# Kernel registry: full kernel definitions ------------------------------------
# A "kernel definition" extends the metadata with component-level d/p/q/r.
# This enables adding new kernels by only dropping a new file into inst/kernels/.

.kernel_defs <- new.env(parent = emptyenv())

#' Register kernel definition
#'
#' Registers a full kernel definition (metadata + component d/p/q/r) into the
#' internal registry. This is intended for package-internal use.
#'
#' @param kernel_def A named list with at least: name, params, support,
#'   default_trans, and component functions d/p/q/r.
#'
#' @return Invisibly returns the kernel name.
#'
#' @keywords internal
register_kernel_def <- function(kernel_def) {
  if (!is.list(kernel_def)) stop("`kernel_def` must be a list.", call. = FALSE)
  if (!is.character(kernel_def$name) || length(kernel_def$name) != 1L) {
    stop("kernel_def$name must be a single character string.", call. = FALSE)
  }
  nm <- kernel_def$name

  # Minimal contract checks (keep light; let unit tests be stricter).
  required_fields <- c("name", "params", "support", "default_trans", "d", "p", "q", "r")
  missing <- setdiff(required_fields, names(kernel_def))
  if (length(missing)) {
    stop("Kernel '", nm, "' is missing fields: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!is.function(kernel_def$d) || !is.function(kernel_def$p) ||
      !is.function(kernel_def$q) || !is.function(kernel_def$r)) {
    stop("Kernel '", nm, "': d/p/q/r must be functions.", call. = FALSE)
  }

  .kernel_defs[[nm]] <- kernel_def

  # Keep the lightweight metadata registry in sync.
  .kernel_registry[[nm]] <- list(
    name = nm,
    params = kernel_def$params,
    support = kernel_def$support,
    default_trans = kernel_def$default_trans
  )

  invisible(nm)
}

#' Get kernel definition
#'
#' @param kernel Kernel name.
#'
#' @return A kernel definition list.
#'
#' @keywords internal
get_kernel_def <- function(kernel) {
  kd <- .kernel_defs[[kernel]]
  if (is.null(kd)) {
    # fall back to metadata-only object to preserve backward compatibility
    return(get_kernel(kernel))
  }
  kd
}
