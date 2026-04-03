#' Laplace (double exponential) mixture distribution
#'
#' Finite mixture of Laplace components for real-valued bulk modeling. The scalar functions in this
#' topic are the NIMBLE-compatible building blocks for Laplace-based kernels.
#'
#' The mixture density is
#' \deqn{
#' f(x) = \sum_{k = 1}^K \tilde{w}_k f_{Lap}(x \mid \mu_k, b_k),
#' }
#' with normalized weights \eqn{\tilde{w}_k}. For vectorized R usage, use [laplace_lowercase()].
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w}
#'   internally when needed.
#' @param location Numeric vector of length \eqn{K} giving component locations.
#' @param scale Numeric vector of length \eqn{K} giving component scales.
#' @param log Logical; if \code{TRUE}, return the log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return Density/CDF/RNG functions return numeric scalars. `qLaplaceMix()` returns a numeric
#'   vector with the same length as `p`.
#'
#' @seealso [laplace_MixGpd()], [laplace_gpd()], [laplace_lowercase()],
#'   [build_nimble_bundle()], [kernel_support_table()].
#' @family laplace kernel families
#'
#' @examples
#' w <- c(0.50, 0.30, 0.20)
#' location <- c(-1, 0.5, 2.0)
#' scale <- c(1.0, 0.7, 1.4)
#'
#' dLaplaceMix(0.8, w = w, location = location, scale = scale, log = FALSE)
#' pLaplaceMix(0.8, w = w, location = location, scale = scale,
#'            lower.tail = TRUE, log.p = FALSE)
#' qLaplaceMix(0.50, w = w, location = location, scale = scale)
#' qLaplaceMix(0.95, w = w, location = location, scale = scale)
#' replicate(10, rLaplaceMix(1, w = w, location = location, scale = scale))
#' @rdname laplace_mix
#' @name laplace_mix
#' @aliases dLaplaceMix pLaplaceMix rLaplaceMix qLaplaceMix
#' @importFrom stats runif uniroot
#' @importFrom nimble ddexp pdexp rdexp qdexp
NULL

#' @describeIn laplace_mix Laplace mixture density
#' @export
dLaplaceMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    K <- length(w)
    wsum <- sum(w)
    if (wsum <= 0.0) {
      if (log == 1) return(-1.0e300) else return(0.0)
    }
    ww <- w / wsum

    s0 <- 0.0
    for (j in 1:K) {
      s0 <- s0 + ww[j] * ddexp(x, location[j], scale[j], log = 0)
    }

    if (log == 1) return(log(s0))
    return(s0)
  }
)

#' @describeIn laplace_mix Laplace mixture distribution function
#' @export
pLaplaceMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    K <- length(w)
    wsum <- sum(w)
    if (wsum <= 0.0) {
      if (log.p == 1) return(-1.0e300) else return(0.0)
    }
    ww <- w / wsum

    cdf <- 0.0
    for (j in 1:K) {
      cdf <- cdf + ww[j] * pdexp(q, location[j], scale[j], lower.tail = 1, log.p = 0)
    }

    if (is.nan(cdf)) cdf <- 0.0
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn laplace_mix Laplace mixture random generation
#' @export
rLaplaceMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    K <- length(w)
    wsum <- sum(w)
    if (wsum <= 0.0) return(0.0)

    u <- runif(1, 0.0, wsum)
    cw <- 0.0
    idx <- 1
    found <- 0

    for (j in 1:K) {
      cw <- cw + w[j]
      if (found == 0) {
        if (u <= cw) {
          idx <- j
          found <- 1
        }
      }
    }

    return(rdexp(1, location[idx], scale[idx]))
  }
)



#' @describeIn laplace_mix Laplace mixture quantile function
#' @export
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param maxiter Integer maximum iterations for \code{stats::uniroot}.
qLaplaceMix <- function(p, w, location, scale,
                        lower.tail = TRUE, log.p = FALSE,
                        tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- -Inf; next }
    if (pi >= 1) { out[i] <- Inf; next }

    lo <- min(nimble::qdexp(pi, location, scale, lower.tail = TRUE, log.p = FALSE), na.rm = TRUE)
    hi <- max(nimble::qdexp(pi, location, scale, lower.tail = TRUE, log.p = FALSE), na.rm = TRUE)
    if (!is.finite(lo)) lo <- -1e20
    if (!is.finite(hi)) hi <- 1e20
    f_lo <- as.numeric(pLaplaceMix(lo, w = w, location, scale, lower.tail = TRUE, log.p = FALSE) - pi)
    f_hi <- as.numeric(pLaplaceMix(hi, w = w, location, scale, lower.tail = TRUE, log.p = FALSE) - pi)
    iter <- 0L
    while (is.finite(f_lo) && f_lo > 0 && lo > -1e20 && iter < 60L) {
      step <- max(1, abs(lo))
      lo <- lo - step
      f_lo <- as.numeric(pLaplaceMix(lo, w = w, location, scale, lower.tail = TRUE, log.p = FALSE) - pi)
      iter <- iter + 1L
    }
    iter <- 0L
    while (is.finite(f_hi) && f_hi < 0 && hi < 1e20 && iter < 60L) {
      step <- max(1, abs(hi))
      hi <- hi + step
      f_hi <- as.numeric(pLaplaceMix(hi, w = w, location, scale, lower.tail = TRUE, log.p = FALSE) - pi)
      iter <- iter + 1L
    }
    if (!is.finite(lo) || !is.finite(hi) || lo >= hi || !is.finite(f_lo) || !is.finite(f_hi) || f_lo * f_hi > 0) {
      out[i] <- NA_real_
    } else {
      out[i] <- stats::uniroot(
        function(z) pLaplaceMix(z, w = w,  location, scale,
                                lower.tail = TRUE, log.p = FALSE) - pi,
        interval = c(lo, hi),
        tol = tol, maxiter = maxiter
      )$root
    }
  }
  out
}

meanLaplaceMix <- function(w, location, scale) {
  ww <- .mean_norm_mix_weights(w)
  if (is.null(ww)) return(NA_real_)
  loc <- as.numeric(location)
  scl <- as.numeric(scale)
  if (length(loc) != length(ww) || length(scl) != length(ww)) return(NA_real_)
  if (any(!is.finite(loc)) || any(!is.finite(scl)) || any(scl <= 0)) return(NA_real_)
  sum(ww * loc)
}

meanLaplaceMixTrunc <- function(w, location, scale, threshold) {
  ww <- .mean_norm_mix_weights(w)
  if (is.null(ww)) return(NA_real_)
  loc <- as.numeric(location)
  scl <- as.numeric(scale)
  u <- as.numeric(threshold)[1]
  if (length(loc) != length(ww) || length(scl) != length(ww)) return(NA_real_)
  if (any(!is.finite(loc)) || any(!is.finite(scl)) || any(scl <= 0) || is.na(u)) return(NA_real_)
  if (is.infinite(u) && u > 0) return(meanLaplaceMix(w = ww, location = loc, scale = scl))
  comp <- ifelse(
    u < loc,
    0.5 * (u - scl) * exp((u - loc) / scl),
    loc - 0.5 * (u + scl) * exp(-(u - loc) / scl)
  )
  sum(ww * comp)
}


#' Laplace mixture with a GPD tail
#'
#' Spliced bulk-tail family formed by attaching a generalized Pareto tail to a Laplace mixture
#' bulk.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param location Numeric vector of length \eqn{K} giving component locations.
#' @param scale Numeric vector of length \eqn{K} giving component scales.
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. `qLaplaceMixGpd()` returns a
#'   numeric vector with the same length as `p`.
#'
#' @seealso [laplace_mix()], [laplace_gpd()], [gpd()], [laplace_lowercase()], [dpmgpd()].
#' @family laplace kernel families
#'
#' @examples
#' w <- c(0.50, 0.30, 0.20)
#' location <- c(-1, 0.5, 2.0)
#' scale <- c(1.0, 0.7, 1.4)
#' threshold <- 1
#' tail_scale <- 1.0
#' tail_shape <- 0.2
#'
#' dLaplaceMixGpd(2.0, w = w, location = location, scale = scale,
#'               threshold = threshold, tail_scale = tail_scale,
#'               tail_shape = tail_shape, log = FALSE)
#' pLaplaceMixGpd(2.0, w = w, location = location, scale = scale,
#'               threshold = threshold, tail_scale = tail_scale,
#'               tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE)
#' qLaplaceMixGpd(0.50, w = w, location = location, scale = scale,
#'               threshold = threshold, tail_scale = tail_scale,
#'               tail_shape = tail_shape)
#' qLaplaceMixGpd(0.95, w = w, location = location, scale = scale,
#'               threshold = threshold, tail_scale = tail_scale,
#'               tail_shape = tail_shape)
#' replicate(10, rLaplaceMixGpd(1, w = w, location = location, scale = scale,
#'                             threshold = threshold,
#'                             tail_scale = tail_scale,
#'                             tail_shape = tail_shape))
#' @rdname laplace_MixGpd
#' @name laplace_MixGpd
#' @aliases dLaplaceMixGpd pLaplaceMixGpd rLaplaceMixGpd qLaplaceMixGpd
NULL

#' @describeIn laplace_MixGpd Laplace mixture + GPD tail density
#' @export
dLaplaceMixGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    if (x < threshold) return(dLaplaceMix(x, w, location, scale, log))

    Fu <- pLaplaceMix(threshold, w, location, scale, 1, 0)
    val <- (1.0 - Fu) * dGpd(x, threshold, scale = tail_scale, shape = tail_shape, log = 0)

    if (log == 1) return(log(val))
    return(val)
  }
)

#' @describeIn laplace_MixGpd Laplace mixture + GPD tail distribution function
#' @export
pLaplaceMixGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    # FIXED: below threshold must call pLaplaceMix(..., scale = scale), not tail_scale
    if (q < threshold) return(pLaplaceMix(q, w, location, scale, lower.tail, log.p))

    Fu <- pLaplaceMix(threshold, w, location, scale, 1, 0)
    G  <- pGpd(q, threshold, scale = tail_scale, shape = tail_shape, lower.tail = 1, log.p = 0)

    cdf <- Fu + (1.0 - Fu) * G

    if (is.nan(cdf)) cdf <- 0.0
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn laplace_MixGpd Laplace mixture + GPD tail random generation
#' @export
rLaplaceMixGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pLaplaceMix(threshold, w, location, scale, 1, 0)
    u  <- runif(1, 0.0, 1.0)

    if (u < Fu) return(rLaplaceMix(1, w, location, scale))
    return(rGpd(1, threshold, scale = tail_scale, shape = tail_shape))
  }
)



#' @describeIn laplace_MixGpd Laplace mixture + GPD tail quantile function
#' @export
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param maxiter Integer maximum iterations for \code{stats::uniroot}.
qLaplaceMixGpd <- function(p, w, location, scale, threshold, tail_scale, tail_shape,
                           lower.tail = TRUE, log.p = FALSE,
                           tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- pLaplaceMix(threshold, w, location, scale, lower.tail = TRUE, log.p = FALSE)
  out <- numeric(length(p))

  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- qLaplaceMix(pi, w, location, scale, lower.tail = TRUE, log.p = FALSE,
                            tol = tol, maxiter = maxiter)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g,  threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}


#' Laplace with a GPD tail
#'
#' Spliced family obtained by attaching a generalized Pareto tail above `threshold` to a single
#' Laplace bulk.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param location Numeric scalar location parameter for the Laplace bulk.
#' @param scale Numeric scalar scale parameter for the Laplace bulk.
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. `qLaplaceGpd()` returns a
#'   numeric vector with the same length as `p`.
#'
#' @seealso [laplace_mix()], [laplace_MixGpd()], [gpd()], [laplace_lowercase()].
#' @family laplace kernel families
#'
#' @examples
#' location <- 0.5
#' scale <- 1.0
#' threshold <- 1
#' tail_scale <- 1.0
#' tail_shape <- 0.2
#'
#' dLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape, log = FALSE)
#' pLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape,
#'            lower.tail = TRUE, log.p = FALSE)
#' qLaplaceGpd(0.50, location, scale, threshold, tail_scale, tail_shape)
#' qLaplaceGpd(0.95, location, scale, threshold, tail_scale, tail_shape)
#' replicate(10, rLaplaceGpd(1, location, scale, threshold, tail_scale, tail_shape))
#' @rdname laplace_gpd
#' @name laplace_gpd
#' @aliases dLaplaceGpd pLaplaceGpd rLaplaceGpd qLaplaceGpd
NULL

#' @describeIn laplace_gpd Laplace + GPD tail density
#' @export
dLaplaceGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 location = double(0),
                 scale = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    if (x < threshold) return(ddexp(x, location, scale, log = log))

    Fu <- pdexp(threshold, location, scale, lower.tail = 1, log.p = 0)
    val <- (1.0 - Fu) * dGpd(x, threshold, scale = tail_scale, shape = tail_shape, log = 0)

    if (log == 1) return(log(val))
    return(val)
  }
)

#' @describeIn laplace_gpd Laplace + GPD tail distribution function
#' @export
pLaplaceGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 location = double(0),
                 scale = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    if (q < threshold) return(pdexp(q, location, scale, lower.tail = lower.tail, log.p = log.p))

    Fu <- pdexp(threshold, location, scale, lower.tail = 1, log.p = 0)
    G  <- pGpd(q, threshold, scale = tail_scale, shape = tail_shape, lower.tail = 1, log.p = 0)

    cdf <- Fu + (1.0 - Fu) * G

    if (is.nan(cdf)) cdf <- 0.0
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn laplace_gpd Laplace + GPD tail random generation
#' @export
rLaplaceGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 location = double(0),
                 scale = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pdexp(threshold, location, scale, lower.tail = 1, log.p = 0)
    u  <- runif(1, 0.0, 1.0)

    if (u < Fu) return(rdexp(1, location, scale))
    return(rGpd(1, threshold, scale = tail_scale, shape = tail_shape))
  }
)



#' @describeIn laplace_gpd Laplace + GPD tail quantile function
#' @export
qLaplaceGpd <- function(p, location, scale, threshold, tail_scale, tail_shape, lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- nimble::pdexp(threshold,  location, scale, lower.tail = TRUE, log.p = FALSE)
  out <- numeric(length(p))

  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- nimble::qdexp(pi,  location, scale, lower.tail = TRUE, log.p = FALSE)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g,  threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}


# ==========================================================
# Lowercase vectorized R wrappers for Laplace kernels
# ==========================================================

#' Lowercase vectorized Laplace distribution functions
#'
#' Vectorized R wrappers for the scalar Laplace-kernel topics in this file.
#'
#' @param x Numeric vector of quantiles.
#' @param q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Integer number of observations to generate.
#' @param w Numeric vector of mixture weights.
#' @param location,scale Numeric vectors (mix) or scalars (base+gpd) of component parameters.
#' @param threshold,tail_scale,tail_shape GPD tail parameters (scalars).
#' @param log Logical; if \code{TRUE}, return log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le x)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are on log scale.
#' @param tol,maxiter Tolerance and max iterations for numerical inversion.
#'
#' @return Numeric vector of densities, probabilities, quantiles, or random variates.
#'
#' @seealso [laplace_mix()], [laplace_MixGpd()], [laplace_gpd()], [bundle()],
#'   [get_kernel_registry()].
#' @family vectorized kernel helpers
#'
#' @examples
#' w <- c(0.6, 0.3, 0.1)
#' loc <- c(0, 1, -2)
#' scl <- c(1, 0.9, 1.1)
#'
#' # Laplace mixture
#' dlaplacemix(c(-1, 0, 1), w = w, location = loc, scale = scl)
#' rlaplacemix(5, w = w, location = loc, scale = scl)
#'
#' @name laplace_lowercase
#' @rdname laplace_lowercase
NULL

# ---- Laplace Mix lowercase wrappers ----

#' @describeIn laplace_lowercase Laplace mixture density (vectorized)
#' @export
dlaplacemix <- function(x, w, location, scale, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dLaplaceMix(xi, w = w, location = location, scale = scale, log = log_int)),
         numeric(1L))
}

#' @describeIn laplace_lowercase Laplace mixture distribution function (vectorized)
#' @export
plaplacemix <- function(q, w, location, scale, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pLaplaceMix(qi, w = w, location = location, scale = scale,
                                                 lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn laplace_lowercase Laplace mixture quantile function (vectorized)
#' @export
qlaplacemix <- function(p, w, location, scale, lower.tail = TRUE, log.p = FALSE,
                        tol = 1e-10, maxiter = 200) {
  qLaplaceMix(p, w = w, location = location, scale = scale, lower.tail = lower.tail,
              log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn laplace_lowercase Laplace mixture random generation (vectorized)
#' @export
rlaplacemix <- function(n, w, location, scale) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rLaplaceMix(1L, w = w, location = location, scale = scale)),
         numeric(1L))
}

# ---- Laplace Mix + GPD lowercase wrappers ----

#' @describeIn laplace_lowercase Laplace mixture + GPD density (vectorized)
#' @export
dlaplacemixgpd <- function(x, w, location, scale, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dLaplaceMixGpd(xi, w = w, location = location, scale = scale,
                                                    threshold = threshold, tail_scale = tail_scale,
                                                    tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn laplace_lowercase Laplace mixture + GPD distribution function (vectorized)
#' @export
plaplacemixgpd <- function(q, w, location, scale, threshold, tail_scale, tail_shape,
                           lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pLaplaceMixGpd(qi, w = w, location = location, scale = scale,
                                                    threshold = threshold, tail_scale = tail_scale,
                                                    tail_shape = tail_shape,
                                                    lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn laplace_lowercase Laplace mixture + GPD quantile function (vectorized)
#' @export
qlaplacemixgpd <- function(p, w, location, scale, threshold, tail_scale, tail_shape,
                           lower.tail = TRUE, log.p = FALSE, tol = 1e-10, maxiter = 200) {
  qLaplaceMixGpd(p, w = w, location = location, scale = scale, threshold = threshold,
                 tail_scale = tail_scale, tail_shape = tail_shape,
                 lower.tail = lower.tail, log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn laplace_lowercase Laplace mixture + GPD random generation (vectorized)
#' @export
rlaplacemixgpd <- function(n, w, location, scale, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rLaplaceMixGpd(1L, w = w, location = location, scale = scale,
                                                           threshold = threshold, tail_scale = tail_scale,
                                                           tail_shape = tail_shape)),
         numeric(1L))
}

# ---- Laplace + GPD lowercase wrappers ----

#' @describeIn laplace_lowercase Laplace + GPD density (vectorized)
#' @export
dlaplacegpd <- function(x, location, scale, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dLaplaceGpd(xi, location = location, scale = scale,
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn laplace_lowercase Laplace + GPD distribution function (vectorized)
#' @export
plaplacegpd <- function(q, location, scale, threshold, tail_scale, tail_shape,
                        lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pLaplaceGpd(qi, location = location, scale = scale,
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape,
                                                 lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn laplace_lowercase Laplace + GPD quantile function (vectorized)
#' @export
qlaplacegpd <- function(p, location, scale, threshold, tail_scale, tail_shape,
                        lower.tail = TRUE, log.p = FALSE) {
  qLaplaceGpd(p, location = location, scale = scale, threshold = threshold,
              tail_scale = tail_scale, tail_shape = tail_shape,
              lower.tail = lower.tail, log.p = log.p)
}

#' @describeIn laplace_lowercase Laplace + GPD random generation (vectorized)
#' @export
rlaplacegpd <- function(n, location, scale, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rLaplaceGpd(1L, location = location, scale = scale,
                                                         threshold = threshold, tail_scale = tail_scale,
                                                         tail_shape = tail_shape)),
         numeric(1L))
}


