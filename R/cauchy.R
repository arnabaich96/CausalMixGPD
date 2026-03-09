#' Cauchy mixture distribution
#'
#' Finite mixture of Cauchy components for symmetric heavy-tailed bulk modeling on the real line.
#'
#' The mixture density is
#' \deqn{
#' f(x) = \sum_{k = 1}^K \tilde{w}_k f_C(x \mid \ell_k, s_k),
#' }
#' with normalized weights \eqn{\tilde{w}_k}. These scalar functions are NIMBLE-compatible; for
#' vectorized R usage, use [cauchy_mix_lowercase()].
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w}
#'   internally when needed.
#' @param location,scale Numeric vectors of length \eqn{K} giving component locations and scales.
#' @param log Logical; if \code{TRUE}, return the log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Density/CDF/RNG functions return numeric scalars. `qCauchyMix()` returns a numeric
#'   vector with the same length as `p`.
#'
#' @seealso [cauchy()], [cauchy_mix_lowercase()], [build_nimble_bundle()], [kernel_support_table()].
#' @family cauchy kernel families
#'
#' @examples
#' w <- c(0.50, 0.30, 0.20)
#' location <- c(-2, 0, 3)
#' scale <- c(1.0, 0.7, 1.5)
#'
#' dCauchyMix(0.5, w = w, location = location, scale = scale, log = FALSE)
#' pCauchyMix(0.5, w = w, location = location, scale = scale,
#'            lower.tail = TRUE, log.p = FALSE)
#' qCauchyMix(0.50, w = w, location = location, scale = scale)
#' qCauchyMix(0.95, w = w, location = location, scale = scale)
#' replicate(10, rCauchyMix(1, w = w, location = location, scale = scale))

#'
#' @rdname cauchy_mix
#' @name cauchy_mix
#' @aliases dCauchyMix pCauchyMix rCauchyMix qCauchyMix
#' @importFrom stats dcauchy pcauchy rcauchy qcauchy runif uniroot
NULL

#' @describeIn cauchy_mix Cauchy mixture density
#' @export
dCauchyMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    K <- length(w)

    # sum weights (allow unnormalized weights)
    wsum <- 0.0
    for (j in 1:K) wsum <- wsum + w[j]
    if (wsum <= 0.0) {
      if (log == 1) return(-1.0e300) else return(0.0)
    }

    s0 <- 0.0
    for (j in 1:K) {
      if (scale[j] > 0.0) {
        z <- (x - location[j]) / scale[j]
        dj <- 1.0 / (pi * scale[j] * (1.0 + z * z))
        s0 <- s0 + (w[j] / wsum) * dj
      }
    }

    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0))
    return(s0)
  }
)

#' @describeIn cauchy_mix Cauchy mixture distribution function
#' @export
pCauchyMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    K <- length(w)

    wsum <- 0.0
    for (j in 1:K) wsum <- wsum + w[j]
    if (wsum <= 0.0) {
      if (log.p != 0) return(log(eps)) else return(eps)
    }

    cdf <- 0.0
    for (j in 1:K) {
      if (scale[j] > 0.0) {
        z <- (q - location[j]) / scale[j]
        pj <- 0.5 + atan(z) / pi
        cdf <- cdf + (w[j] / wsum) * pj
      }
    }

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) return(log(max(cdf, eps)))
    return(cdf)
  }
)

#' @describeIn cauchy_mix Cauchy mixture random generation
#' @export
rCauchyMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1)) {
    returnType(double(0))

    if (n != 1) return(0.0)
    K <- length(w)

    wsum <- 0.0
    for (j in 1:K) wsum <- wsum + w[j]
    if (wsum <= 0.0) return(0.0)

    # component draw using unnormalized weights
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

    if (scale[idx] <= 0.0) return(0.0)

    # inverse-CDF sampling
    uu <- runif(1, 0.0, 1.0)
    return(location[idx] + scale[idx] * tan(pi * (uu - 0.5)))
  }
)


#' @describeIn cauchy_mix Cauchy mixture quantile function
#' @export
qCauchyMix <- function(p, w, location, scale,
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

    lo <- min(stats::qcauchy(pi, location = location, scale = scale), na.rm = TRUE)
    hi <- max(stats::qcauchy(pi, location = location, scale = scale), na.rm = TRUE)
    if (!is.finite(lo)) lo <- -1e20
    if (!is.finite(hi)) hi <- 1e20
    f_lo <- as.numeric(pCauchyMix(lo, w = w, location = location, scale = scale,
                                 lower.tail = TRUE, log.p = FALSE) - pi)
    f_hi <- as.numeric(pCauchyMix(hi, w = w, location = location, scale = scale,
                                 lower.tail = TRUE, log.p = FALSE) - pi)
    iter <- 0L
    while (is.finite(f_lo) && f_lo > 0 && lo > -1e20 && iter < 60L) {
      step <- max(1, abs(lo))
      lo <- lo - step
      f_lo <- as.numeric(pCauchyMix(lo, w = w, location = location, scale = scale,
                                   lower.tail = TRUE, log.p = FALSE) - pi)
      iter <- iter + 1L
    }
    iter <- 0L
    while (is.finite(f_hi) && f_hi < 0 && hi < 1e20 && iter < 60L) {
      step <- max(1, abs(hi))
      hi <- hi + step
      f_hi <- as.numeric(pCauchyMix(hi, w = w, location = location, scale = scale,
                                   lower.tail = TRUE, log.p = FALSE) - pi)
      iter <- iter + 1L
    }
    if (!is.finite(lo) || !is.finite(hi) || lo >= hi || !is.finite(f_lo) || !is.finite(f_hi) || f_lo * f_hi > 0) {
      out[i] <- NA_real_
    } else {
      out[i] <- stats::uniroot(
        function(z) pCauchyMix(z, w = w, location = location, scale = scale,
                               lower.tail = TRUE, log.p = FALSE) - pi,
        interval = c(lo, hi),
        tol = tol, maxiter = maxiter
      )$root
    }
  }
  out
}


# ==========================================================
# Lowercase vectorized R wrappers for Cauchy mixture
# ==========================================================

#' Lowercase vectorized Cauchy mixture distribution functions
#'
#' Vectorized R wrappers for the scalar Cauchy mixture functions in this file.
#'
#' @param x Numeric vector of quantiles.
#' @param q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Integer number of observations to generate.
#' @param w Numeric vector of mixture weights.
#' @param location,scale Numeric vectors of component parameters.
#' @param log Logical; if \code{TRUE}, return log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le x)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are on log scale.
#' @param tol,maxiter Tolerance and max iterations for numerical inversion.
#'
#' @return Numeric vector of densities, probabilities, quantiles, or random variates.
#'
#' @seealso [cauchy_mix()], [cauchy()], [bundle()], [get_kernel_registry()].
#' @family vectorized kernel helpers
#'
#' @examples
#' w <- c(0.6, 0.3, 0.1)
#' loc <- c(-1, 0, 1)
#' scl <- c(1, 1.2, 2)
#'
#' dcauchymix(c(-2, 0, 2), w = w, location = loc, scale = scl)
#' rcauchymix(5, w = w, location = loc, scale = scl)
#'
#' @name cauchy_mix_lowercase
#' @rdname cauchy_mix_lowercase
NULL

#' @describeIn cauchy_mix_lowercase Cauchy mixture density (vectorized)
#' @export
dcauchymix <- function(x, w, location, scale, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dCauchyMix(xi, w = w, location = location, scale = scale, log = log_int)),
         numeric(1L))
}

#' @describeIn cauchy_mix_lowercase Cauchy mixture distribution function (vectorized)
#' @export
pcauchymix <- function(q, w, location, scale, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pCauchyMix(qi, w = w, location = location, scale = scale,
                                                lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn cauchy_mix_lowercase Cauchy mixture quantile function (vectorized)
#' @export
qcauchymix <- function(p, w, location, scale, lower.tail = TRUE, log.p = FALSE,
                       tol = 1e-10, maxiter = 200) {
  qCauchyMix(p, w = w, location = location, scale = scale, lower.tail = lower.tail,
             log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn cauchy_mix_lowercase Cauchy mixture random generation (vectorized)
#' @export
rcauchymix <- function(n, w, location, scale) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rCauchyMix(1L, w = w, location = location, scale = scale)),
         numeric(1L))
}
