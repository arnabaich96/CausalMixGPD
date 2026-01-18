#' Cauchy mixture distribution
#'
#' A finite mixture of Cauchy components. Base Cauchy functions are taken from \pkg{stats}.
#' The mixture density and distribution function are computed by weighted sums. Random generation
#' samples a component index according to the weights and draws from the corresponding component.
#' Quantiles are computed by numerical inversion of the mixture CDF.
#' The \code{d*}, \code{p*}, and \code{q*} functions accept vector inputs for their first argument
#' and evaluate elementwise; \code{r*} supports \code{n > 1}.
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
#' @return Density/CDF/RNG functions return numeric scalars. \code{qCauchyMix} returns a numeric vector
#'   with the same length as \code{p}.
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

