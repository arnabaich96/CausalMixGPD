#' Generalized Pareto distribution
#'
#' The generalized Pareto distribution (GPD) is the standard model for threshold exceedances in
#' peaks-over-threshold methods. In this package it provides the tail component used to splice
#' onto a bulk model. The implementation follows the common threshold parameterization
#' \eqn{X \ge u} with scale \code{sigma} and shape \code{xi}.
#'
#' The density, distribution, and random-generation functions are implemented as
#' \code{nimbleFunction}s for use inside NIMBLE models. The quantile function is an R function
#' and is also used internally by mixture+GPD quantile functions.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param u Numeric scalar threshold parameter. Values below \code{u} have zero density under the GPD.
#' @param sigma Numeric scalar scale parameter; must be positive.
#' @param xi Numeric scalar shape parameter. When \code{xi < 0}, the distribution has a finite upper endpoint.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return \code{dGpd} returns a numeric scalar density; \code{pGpd} returns a numeric scalar CDF;
#'   \code{qGpd} returns a numeric vector of quantiles; \code{rGpd} returns one random draw.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' d <- compileNimble(dGpd)
#' d(3, u = 2, sigma = 1, xi = 0.2)
#' qGpd(c(0.5, 0.9), u = 2, sigma = 1, xi = 0.2)
#' }
#'
#' @rdname GPD
#' @name GPD
#' @aliases dGpd pGpd rGpd qGpd
NULL
#' DENITY of GPD with threshold u
#' @describeIn GPD Density of GPD with threshold u
#' @export
dGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    # basic domain checks only (no is.finite, no any/all)
    if (sigma <= 0.0) {
      if (log == 1) return(log(eps)) else return(eps)
    }
    if (x < u) {
      if (log == 1) return(log(eps)) else return(eps)
    }

    z <- (x - u) / sigma

    if (abs(xi) > 1e-12) {
      t <- 1.0 + xi * z
      if (t <= 0.0) { # outside support when xi < 0
        if (log == 1) return(log(eps)) else return(eps)
      }
      val <- (1.0 / sigma) * pow(t, -(1.0 + 1.0/xi))
      if (val < eps) val <- eps
      if (log == 1) return(log(val)) else return(val)
    } else {
      val <- (1.0 / sigma) * exp(-z)
      if (val < eps) val <- eps
      if (log == 1) return(log(val)) else return(val)
    }
  }
)

#' @describeIn GPD density of GPD with threshold u
#' @export
pGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (sigma <= 0.0) {
      if (log.p != 0) return(log(eps)) else return(eps)
    }
    if (q < u) {
      # CDF below threshold is 0
      if (lower.tail == 1) {
        if (log.p != 0) return(log(eps)) else return(0.0)
      } else {
        if (log.p != 0) return(log(1.0)) else return(1.0)
      }
    }

    z <- (q - u) / sigma

    if (abs(xi) > 1e-12) {
      t <- 1.0 + xi * z
      if (t <= 0.0) {
        # beyond upper endpoint for xi < 0 => CDF is 1
        cdf <- 1.0
      } else {
        cdf <- 1.0 - pow(t, -1.0/xi)
      }
    } else {
      cdf <- 1.0 - exp(-z)
    }

    # clamp
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn GPD Random number Generator
#' @export
rGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0)) {
    returnType(double(0))
    if (n != 1) return(u)
    # IMPORTANT: use base::runif inside nimbleFunction (not nimble::runif)
    uu <- runif(1, 0.0, 1.0)

    if (sigma <= 0.0) return(u)

    if (abs(xi) > 1e-12) {
      return(u + (sigma/xi) * (pow(1.0 - uu, -xi) - 1.0))
    } else {
      return(u - sigma * log(1.0 - uu))
    }
  }
)


#' @describeIn GPD Quantile function
#' @export
qGpd <- function(p, u, sigma, xi, lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  if (!is.finite(u) || !is.finite(sigma) || !is.finite(xi)) stop("u, sigma, xi must be finite.", call. = FALSE)
  if (sigma <= 0) stop("sigma must be > 0.", call. = FALSE)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- u; next }
    if (pi >= 1) { out[i] <- if (xi < 0) (u - sigma/xi) else Inf; next }
    if (abs(xi) < 1e-12) out[i] <- u - sigma * log(1 - pi)
    else out[i] <- u + (sigma/xi) * ((1 - pi)^(-xi) - 1)
  }
  out
}

#' @importFrom stats dgamma dlnorm dnorm dt plnorm pnorm pt qgamma rlnorm rnorm rt runif
#' @keywords internal
#' @noRd
NULL

