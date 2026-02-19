#' Kernel Dispatch Utilities (internal)
#'
#' Helper functions for resolving kernel specifications and wrapping distribution
#' functions to handle vector/matrix inputs and alternative parameter naming.
#'
#' @name dispatch-utils
#' @keywords internal
#' @noRd
NULL

#' Resolve kernel dispatch functions (scalar)
#'
#' Dispatch returns raw scalar nimbleFunctions for codegen; do not wrap.
#'
#' @param spec_or_fit mixgpd_fit or spec list
#' @param backend_override Optional backend override
#' @return List with d/p/q/r functions and bulk_params.
#' @keywords internal
#' @noRd
.get_dispatch_scalar <- function(spec_or_fit, backend_override = NULL) {
  spec <- spec_or_fit
  if (inherits(spec_or_fit, "mixgpd_fit")) {
    spec <- spec_or_fit$spec %||% list()
  }

  meta <- spec$meta %||% list()
  backend <- meta$backend %||% spec$dispatch$backend %||% "<unknown>"
  if (!is.null(backend_override)) backend <- backend_override
  kernel <- meta$kernel %||% spec$kernel$key %||% "<unknown>"
  GPD <- isTRUE(meta$GPD %||% spec$dispatch$GPD)

  kdef <- get_kernel_registry()[[kernel]]
  if (is.null(kdef)) stop(sprintf("Kernel '%s' not found in registry.", kernel), call. = FALSE)
  if (isTRUE(GPD) && isFALSE(kdef$allow_gpd)) stop(sprintf("Kernel '%s' does not allow GPD.", kernel), call. = FALSE)

  backend_key <- match.arg(backend, choices = allowed_backends)
  dispatch <- kdef[[backend_key]]
  if (is.null(dispatch)) {
    stop(sprintf("Missing %s dispatch in kernel registry.", backend_key), call. = FALSE)
  }

  d_name <- if (isTRUE(GPD)) {
    dispatch$d_gpd
  } else {
    dispatch$d %||% dispatch$d_base
  }
  if (is.na(d_name) || !nzchar(d_name)) {
    stop(sprintf("Missing %s dispatch for kernel '%s'.", backend_key, kernel), call. = FALSE)
  }

  p_name <- sub("^d", "p", d_name)
  q_name <- sub("^d", "q", d_name)
  r_name <- sub("^d", "r", d_name)

  ns_pkg <- getNamespace("CausalMixGPD")
  ns_stats <- getNamespace("stats")
  ns_nimble <- getNamespace("nimble")

  .resolve_fun <- function(fname, kernel) {
    # Try PascalCase NIMBLE function first (for NIMBLE model code)
    if (exists(fname, envir = ns_pkg, inherits = FALSE)) {
      return(get(fname, envir = ns_pkg))
    }
    if (exists(fname, envir = ns_stats, inherits = FALSE)) {
      return(get(fname, envir = ns_stats))
    }
    if (exists(fname, envir = ns_nimble, inherits = FALSE)) {
      return(get(fname, envir = ns_nimble))
    }
    # Fallback for predictions: use lowercase R wrappers if PascalCase NIMBLE function
    # not found (e.g., in some build contexts). Lowercase wrappers are vectorized
    # and work for predictions but should not be used in NIMBLE model code.
    fname_lower <- tolower(fname)
    if (exists(fname_lower, envir = ns_pkg, inherits = FALSE)) {
      return(get(fname_lower, envir = ns_pkg))
    }
    stop(sprintf("Missing function '%s' for kernel '%s'.", fname, kernel), call. = FALSE)
  }

  d_fun <- .wrap_density_fun(.resolve_fun(d_name, kernel))
  p_fun <- .wrap_cdf_fun(.resolve_fun(p_name, kernel))
  q_fun <- .wrap_quantile_fun(.resolve_fun(q_name, kernel))
  r_fun <- .wrap_rng_fun(.resolve_fun(r_name, kernel))

  if (isTRUE(attr(d_fun, "vectorized_wrapper")) ||
      isTRUE(attr(p_fun, "vectorized_wrapper")) ||
      isTRUE(attr(q_fun, "vectorized_wrapper")) ||
      isTRUE(attr(r_fun, "vectorized_wrapper"))) {
    stop("Scalar dispatch unexpectedly received vectorized wrappers.", call. = FALSE)
  }

  list(d = d_fun, p = p_fun, q = q_fun, r = r_fun, bulk_params = kdef$bulk_params)
}

#' Resolve kernel dispatch functions
#'
#' Dispatch returns vector-aware d/p/q and n-aware r via wrappers; do not mutate namespace.
#'
#' @param spec_or_fit mixgpd_fit or spec list
#' @param backend_override Optional backend override
#' @return List with d/p/q/r functions and bulk_params.
#' @keywords internal
#' @noRd
.get_dispatch <- function(spec_or_fit, backend_override = NULL) {
  scalar <- .get_dispatch_scalar(spec_or_fit, backend_override = backend_override)
  list(
    d = .wrap_scalar_first_arg(scalar$d, "x"),
    p = .wrap_scalar_p(scalar$p),
    q = .wrap_scalar_first_arg(scalar$q, "p"),
    r = .wrap_scalar_r(scalar$r),
    bulk_params = scalar$bulk_params
  )
}

#' Detect the first present argument name in dots
#'
#' @param dots List of arguments
#' @param candidates Character vector of candidate names
#' @return Character; the name of the first present argument
#' @keywords internal
#' @noRd
.detect_first_present <- function(dots, candidates = c("q", "x")) {
  for (nm in candidates) {
    if (!is.null(dots[[nm]])) return(nm)
  }
  stop("Expected one of: ", paste(candidates, collapse = ", "), call. = FALSE)
}

#' Wrap scalar first-argument functions to handle vector inputs
#'
#' @param fun Scalar nimbleFunction
#' @param first_arg_name Name of the first argument to vectorize over
#' @return Wrapped function with vectorized_wrapper attribute set to TRUE
#' @keywords internal
#' @noRd
.wrap_scalar_first_arg <- function(fun, first_arg_name) {
  if (isTRUE(attr(fun, "vectorized_wrapper"))) return(fun)
  force(fun)
  force(first_arg_name)
  wrapper <- function(...) {
    dots <- list(...)
    if (!first_arg_name %in% names(dots)) {
      stop("Missing required argument: ", first_arg_name, call. = FALSE)
    }
    vec <- dots[[first_arg_name]]
    if (length(vec) <= 1L) return(do.call(fun, dots))

    dots[[first_arg_name]] <- vec[1]
    one <- do.call(fun, dots)
    if (length(one) <= 1L) {
      return(vapply(vec, function(v) {
        dots[[first_arg_name]] <- v
        do.call(fun, dots)
      }, numeric(1)))
    }

    mat <- vapply(vec, function(v) {
      dots[[first_arg_name]] <- v
      as.numeric(do.call(fun, dots))
    }, numeric(length(one)))
    t(mat)
  }
  attr(wrapper, "vectorized_wrapper") <- TRUE
  wrapper
}

#' Wrap scalar CDF to handle q/x naming and vector inputs
#'
#' @param fun Scalar nimbleFunction for CDF
#' @return Wrapped function with vectorized_wrapper attribute set to TRUE
#' @keywords internal
#' @noRd
.wrap_scalar_p <- function(fun) {
  if (isTRUE(attr(fun, "vectorized_wrapper"))) return(fun)
  force(fun)
  wrapper <- function(...) {
    dots <- list(...)
    given <- .detect_first_present(dots, candidates = c("q", "x"))

    formal_names <- names(formals(fun)) %||% character()
    target <- if ("q" %in% formal_names && !"x" %in% formal_names) {
      "q"
    } else if ("x" %in% formal_names && !"q" %in% formal_names) {
      "x"
    } else {
      given
    }

    if (!identical(given, target)) {
      dots[[target]] <- dots[[given]]
      dots[[given]] <- NULL
    }

    vec <- dots[[target]]
    if (length(vec) <= 1L) return(do.call(fun, dots))

    dots[[target]] <- vec[1]
    one <- do.call(fun, dots)
    if (length(one) <= 1L) {
      return(vapply(vec, function(v) {
        dots[[target]] <- v
        do.call(fun, dots)
      }, numeric(1)))
    }

    mat <- vapply(vec, function(v) {
      dots[[target]] <- v
      as.numeric(do.call(fun, dots))
    }, numeric(length(one)))
    t(mat)
  }
  attr(wrapper, "vectorized_wrapper") <- TRUE
  wrapper
}

#' Wrap scalar RNG to handle n > 1
#'
#' @param fun Scalar nimbleFunction for RNG
#' @return Wrapped function with vectorized_wrapper attribute set to TRUE
#' @keywords internal
#' @noRd
.wrap_scalar_r <- function(fun) {
  if (isTRUE(attr(fun, "vectorized_wrapper"))) return(fun)
  force(fun)
  wrapper <- function(...) {
    dots <- list(...)
    if (!("n" %in% names(dots))) stop("Missing required argument: n", call. = FALSE)
    n <- as.integer(dots$n)
    dots$n <- NULL
    if (is.na(n) || n < 0L) stop("n must be a non-negative integer.", call. = FALSE)
    if (n == 0L) {
      one <- do.call(fun, c(list(n = 1L), dots))
      if (length(one) <= 1L) return(numeric(0))
      return(matrix(numeric(0), nrow = 0, ncol = length(one)))
    }
    if (n == 1L) return(do.call(fun, c(list(n = 1L), dots)))

    one <- do.call(fun, c(list(n = 1L), dots))
    if (length(one) <= 1L) {
      return(vapply(seq_len(n), function(i) {
        do.call(fun, c(list(n = 1L), dots))
      }, numeric(1)))
    }

    mat <- vapply(seq_len(n), function(i) {
      as.numeric(do.call(fun, c(list(n = 1L), dots)))
    }, numeric(length(one)))
    t(mat)
  }
  attr(wrapper, "vectorized_wrapper") <- TRUE
  wrapper
}
