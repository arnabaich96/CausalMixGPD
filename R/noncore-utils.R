#' Internal utilities (non-distribution)
#'
#' These helpers implement input checks, design-matrix preparation, and small
#' utilities shared across the model back ends. They are internal and not
#' exported.
#'
#' @name noncore_utils
#' @keywords internal
NULL

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Check formula and data
#'
#' @param formula A model formula with a numeric response.
#' @param data A data.frame containing variables in \code{formula}.
#' @return Invisibly returns \code{TRUE}.
#' @keywords internal
.check_formula_data <- function(formula, data) {
  if (!inherits(formula, "formula")) stop("`formula` must be a formula.", call. = FALSE)
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)

  mf <- stats::model.frame(formula, data = data, na.action = stats::na.fail)
  y <- stats::model.response(mf)
  if (!is.numeric(y)) stop("Response must be numeric.", call. = FALSE)
  invisible(TRUE)
}

#' Check MCMC control list
#'
#' @param mcmc A list with at least \code{niter} and \code{nburn}.
#' @return A normalized list.
#' @keywords internal
.check_mcmc <- function(mcmc) {
  if (!is.list(mcmc)) stop("`mcmc` must be a list.", call. = FALSE)
  niter <- as.integer(mcmc$niter %||% 2000L)
  nburn <- as.integer(mcmc$nburn %||% floor(niter / 2))
  nthin <- as.integer(mcmc$nthin %||% 1L)
  nchains <- as.integer(mcmc$nchains %||% 1L)
  if (niter <= 0L) stop("mcmc$niter must be positive.", call. = FALSE)
  if (nburn < 0L || nburn >= niter) stop("mcmc$nburn must be in [0, niter).", call. = FALSE)
  if (nthin <= 0L) stop("mcmc$nthin must be positive.", call. = FALSE)
  if (nchains <= 0L) stop("mcmc$nchains must be positive.", call. = FALSE)
  list(niter = niter, nburn = nburn, nthin = nthin, nchains = nchains,
       monitors = mcmc$monitors %||% NULL,
       setSeed = mcmc$setSeed %||% NULL)
}

#' Prepare design matrix
#'
#' The package treats models as unconditional if the right-hand side of the
#' formula is empty (for example \code{y ~ 1}).
#'
#' @param formula Model formula.
#' @param data Data frame.
#' @return A list with \code{y}, \code{X}, \code{has_regression}, and \code{terms}.
#' @keywords internal
.prepare_design_matrix <- function(formula, data) {
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.fail)
  y <- stats::model.response(mf)
  mm <- stats::model.matrix(attr(mf, "terms"), data = mf)

  # Drop intercept-only as "unconditional" if it is the only column.
  if (ncol(mm) == 1L && identical(colnames(mm), "(Intercept)")) {
    X <- matrix(numeric(0), nrow(mm), 0L)
    has_regression <- FALSE
  } else {
    X <- mm
    has_regression <- ncol(X) > 0L
  }

  list(y = as.numeric(y), X = X, has_regression = has_regression, terms = stats::terms(formula))
}

#' Prepare newdata design matrix
#'
#' @param newdata Optional data frame.
#' @param fit_terms Terms object from the fitted model.
#' @param X_train Training design matrix (used when \code{newdata} is \code{NULL}).
#' @return A numeric matrix.
#' @keywords internal
.prepare_newdata_matrix <- function(newdata, fit_terms, X_train) {
  if (is.null(newdata)) return(X_train)
  mf <- stats::model.frame(fit_terms, data = newdata, na.action = stats::na.fail)
  mm <- stats::model.matrix(fit_terms, data = mf)
  if (ncol(X_train) == 0L) {
    matrix(numeric(0), nrow(mm), 0L)
  } else {
    mm
  }
}

#' Apply a link/transform
#'
#' @param x Numeric vector.
#' @param trans Character scalar: "identity" or "exp".
#' @return Numeric vector.
#' @keywords internal
.apply_trans <- function(x, trans) {
  if (is.null(trans) || identical(trans, "identity")) return(x)
  if (identical(trans, "exp")) return(exp(x))
  stop("Unknown transform '", trans, "'.", call. = FALSE)
}

#' Summarize draws to a tidy matrix
#'
#' @param draws Numeric matrix with rows as draws.
#' @param prob Numeric vector of length 2 giving lower/upper credible probs.
#' @return A matrix with columns mean, sd, q_low, q_high.
#' @keywords internal
.summarize_draw_matrix <- function(draws, prob = c(0.025, 0.975)) {
  stopifnot(is.matrix(draws))
  mu <- colMeans(draws)
  sd <- apply(draws, 2L, stats::sd)
  qs <- apply(draws, 2L, stats::quantile, probs = prob, names = FALSE)
  out <- cbind(mean = mu, sd = sd, q_low = qs[1L, ], q_high = qs[2L, ])
  out
}


#' Get component-level distribution functions for a kernel
#'
#' @param kernel Kernel name.
#' @return A list with \code{r} and \code{p} component functions. Each function
#'   must accept \code{n} or \code{q} and parameters \code{param1}, \code{param2}
#'   using the canonical Param1/Param2 meaning for that kernel.
#' @keywords internal
.kernel_component_funs <- function(kernel) {
  if (!is.character(kernel) || length(kernel) != 1L) {
    stop("`kernel` must be a single character string.", call. = FALSE)
  }

  if (identical(kernel, "gamma")) {
    rfun <- function(n, param1, param2) stats::rgamma(n, shape = param1, rate = param1 / param2)
    pfun <- function(q, param1, param2) stats::pgamma(q, shape = param1, rate = param1 / param2)
    return(list(r = rfun, p = pfun))
  }

  if (identical(kernel, "normal")) {
    rfun <- function(n, param1, param2) stats::rnorm(n, mean = param1, sd = param2)
    pfun <- function(q, param1, param2) stats::pnorm(q, mean = param1, sd = param2)
    return(list(r = rfun, p = pfun))
  }

  if (identical(kernel, "lognormal")) {
    rfun <- function(n, param1, param2) stats::rlnorm(n, meanlog = param1, sdlog = param2)
    pfun <- function(q, param1, param2) stats::plnorm(q, meanlog = param1, sdlog = param2)
    return(list(r = rfun, p = pfun))
  }

  if (identical(kernel, "cauchy")) {
    rfun <- function(n, param1, param2) stats::rcauchy(n, location = param1, scale = param2)
    pfun <- function(q, param1, param2) stats::pcauchy(q, location = param1, scale = param2)
    return(list(r = rfun, p = pfun))
  }

  if (identical(kernel, "laplace")) {
    rfun <- function(n, param1, param2) {
      u <- stats::runif(n) - 0.5
      param1 - param2 * sign(u) * log1p(-2 * abs(u))
    }
    pfun <- function(q, param1, param2) {
      z <- (q - param1) / param2
      ifelse(q < param1, 0.5 * exp(z), 1 - 0.5 * exp(-z))
    }
    return(list(r = rfun, p = pfun))
  }

  if (identical(kernel, "inverse_gaussian")) {
    rfun <- function(n, param1, param2) {
      f <- get0("rinvgauss", envir = asNamespace("DPmixGPD"), inherits = FALSE)
      if (!is.function(f)) stop("rinvgauss is not available.", call. = FALSE)
      f(n, mean = param1, shape = param2)
    }
    pfun <- function(q, param1, param2) {
      f <- get0("pinvgauss", envir = asNamespace("DPmixGPD"), inherits = FALSE)
      if (!is.function(f)) stop("pinvgauss is not available.", call. = FALSE)
      f(q, mean = param1, shape = param2)
    }
    return(list(r = rfun, p = pfun))
  }

  if (identical(kernel, "pareto")) {
    rfun <- function(n, param1, param2) {
      f <- get0("rPareto", envir = asNamespace("DPmixGPD"), inherits = FALSE)
      if (!is.function(f)) stop("rPareto is not available.", call. = FALSE)
      # Here Param1 = shape, Param2 = scale.
      f(n, shape = param1, scale = param2)
    }
    pfun <- function(q, param1, param2) {
      f <- get0("pPareto", envir = asNamespace("DPmixGPD"), inherits = FALSE)
      if (!is.function(f)) stop("pPareto is not available.", call. = FALSE)
      f(q, shape = param1, scale = param2)
    }
    return(list(r = rfun, p = pfun))
  }

  stop("Unsupported kernel '", kernel, "' for component functions.", call. = FALSE)
}



#' Posterior predictive sampling for a finite mixture
#'
#' @param kernel Kernel name.
#' @param wt Numeric vector of mixture weights (length K).
#' @param param1 Numeric vector of component Param1 (length K).
#' @param param2 Numeric vector of component Param2 (length K).
#' @param n Integer number of samples to draw.
#' @return Numeric vector of length \code{n}.
#' @keywords internal
.pp_sample_mix <- function(kernel, wt, param1, param2, n = 1L) {
  stopifnot(length(wt) == length(param1), length(param1) == length(param2))
  K <- length(wt)
  id <- sample.int(K, size = n, replace = TRUE, prob = wt)
  fns <- .kernel_component_funs(kernel)
  out <- numeric(n)
  for (k in seq_len(K)) {
    sel <- which(id == k)
    if (!length(sel)) next
    out[sel] <- fns$r(length(sel), param1[k], param2[k])
  }
  out
}

#' Posterior predictive sampling for a MixGPD splice
#'
#' This implementation follows a simple splicing approach: with probability
#' \code{F_bulk(u)} draw from the bulk mixture truncated to \code{y <= u}, and
#' otherwise draw an exceedance from the GPD and add \code{u}.
#'
#' @param kernel Kernel name.
#' @param wt Mixture weights.
#' @param param1 Component Param1.
#' @param param2 Component Param2.
#' @param u Threshold.
#' @param sigma GPD scale.
#' @param xi GPD shape.
#' @param n Number of samples.
#' @return Numeric vector of samples.
#' @keywords internal
.pp_sample_mixgpd <- function(kernel, wt, param1, param2, u, sigma, xi, n = 1L) {
  fns <- .kernel_component_funs(kernel)
  # bulk CDF at u
  Fb_u <- sum(wt * vapply(seq_along(wt), function(j) fns$p(u, param1[j], param2[j]), numeric(1)))
  Fb_u <- min(max(Fb_u, 1e-12), 1 - 1e-12)

  out <- numeric(n)
  u_unif <- stats::runif(n)

  # Bulk samples: rejection to enforce <= u
  bulk_n <- sum(u_unif <= Fb_u)
  if (bulk_n > 0L) {
    # draw from mixture until accepted (finite expected loops)
    acc <- numeric(0)
    while (length(acc) < bulk_n) {
      draw <- .pp_sample_mix(kernel, wt, param1, param2, n = bulk_n - length(acc))
      acc <- c(acc, draw[draw <= u])
    }
    out[u_unif <= Fb_u] <- acc[seq_len(bulk_n)]
  }

  # Tail samples
  tail_n <- n - bulk_n
  if (tail_n > 0L) {
    rG <- get0("rGpd", envir = asNamespace("DPmixGPD"), inherits = FALSE)
    if (!is.function(rG)) stop("rGpd is not available.", call. = FALSE)
    out[u_unif > Fb_u] <- u + rG(tail_n, sigma = sigma, xi = xi)
  }

  out
}
