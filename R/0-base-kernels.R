# ==========================================================
# 1) Generalized Pareto distribution (GPD)
# ==========================================================

#' Generalized Pareto distribution
#'
#' Base generalized Pareto distribution (GPD) for threshold exceedances above \code{threshold}.
#' Parameterization uses threshold \code{threshold}, scale \code{scale > 0}, and shape \code{shape}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param threshold Numeric scalar threshold at which the GPD is attached.
#' @param scale Numeric scalar GPD scale parameter; must be positive.
#' @param shape Numeric scalar GPD shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return \code{dGpd} returns a numeric scalar density; \code{pGpd} returns a numeric scalar CDF;
#'   \code{rGpd} returns one random draw; \code{qGpd} returns a numeric quantile.
#'
#' @examples
#' threshold <- 1
#' tail_scale <- 0.8
#' tail_shape <- 0.2
#'
#' dGpd(1.5, threshold, tail_scale, tail_shape, log = 0)
#' pGpd(1.5, threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
#' qGpd(0.50, threshold, tail_scale, tail_shape)
#' qGpd(0.95, threshold, tail_scale, tail_shape)
#' replicate(10, rGpd(1, threshold, tail_scale, tail_shape))
#' @rdname gpd
#' @name gpd
#' @aliases dGpd pGpd rGpd qGpd
#' @importFrom stats runif
NULL

#' @describeIn gpd Generalized Pareto density function
#' @export
dGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 threshold = double(0),
                 scale = double(0),
                 shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (scale <= 0.0) {
      if (log == 1) return(log(eps)) else return(eps)
    }
    if (x < threshold) {
      if (log == 1) return(log(eps)) else return(eps)
    }

    z <- (x - threshold) / scale
    val <- 0.0

    if (abs(shape) < 1e-12) {
      # Exponential limit
      val <- (1.0 / scale) * exp(-z)
    } else {
      t <- 1.0 + shape * z
      if (t <= 0.0) {
        if (log == 1) return(log(eps)) else return(eps)
      }
      val <- (1.0 / scale) * (t ^ (-1.0 / shape - 1.0))
    }

    if (val < eps) val <- eps
    if (log == 1) return(log(val))
    return(val)
  }
)

#' @describeIn gpd Generalized Pareto distribution function
#' @export
pGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 threshold = double(0),
                 scale = double(0),
                 shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (scale <= 0.0) {
      if (log.p != 0) return(log(eps)) else return(eps)
    }

    cdf <- 0.0
    if (q < threshold) {
      cdf <- 0.0
    } else {
      z <- (q - threshold) / scale
      if (abs(shape) < 1e-12) {
        cdf <- 1.0 - exp(-z)
      } else {
        t <- 1.0 + shape * z
        if (t <= 0.0) {
          cdf <- 1.0
        } else {
          cdf <- 1.0 - (t^(-1.0 / shape))
        }
      }
      if (cdf < 0.0) cdf <- 0.0
      if (cdf > 1.0) cdf <- 1.0
    }

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) return(log(max(cdf, eps)))
    return(cdf)
  }
)

#' @describeIn gpd Generalized Pareto random generation
#' @export
rGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 threshold = double(0),
                 scale = double(0),
                 shape = double(0)) {
    returnType(double(0))
    if (n != 1) return(0.0)
    if (scale <= 0.0) return(0.0)

    u <- runif(1, 0.0, 1.0)

    if (abs(shape) < 1e-12) {
      return(threshold - scale * log(1.0 - u))
    }
    return(threshold + (scale / shape) * ((1.0 - u)^(-shape) - 1.0))
  }
)

#' @describeIn gpd Vectorized RNG wrapper (R-only)
#' @export
rGpd_vec <- function(n, threshold, scale, shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0))
  if (n == 1L) return(as.numeric(rGpd(1, threshold, scale, shape)))
  vapply(seq_len(n), function(i) as.numeric(rGpd(1, threshold, scale, shape)), numeric(1))
}

#' @describeIn gpd Generalized Pareto quantile function
#' @export
qGpd <- function(p, threshold, scale, shape,
                 lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- threshold; next }
    if (pi >= 1) {
      if (shape < 0) {
        out[i] <- threshold - scale / shape
      } else {
        out[i] <- Inf
      }
      next
    }

    if (abs(shape) < 1e-12) {
      out[i] <- threshold - scale * log(1.0 - pi)
    } else {
      out[i] <- threshold + (scale / shape) * ((1.0 - pi)^(-shape) - 1.0)
    }
  }
  out
}


# ==========================================================
# 2) Inverse Gaussian (custom base)
# ==========================================================

#' Inverse Gaussian (Wald) distribution
#'
#' The inverse Gaussian (also called the Wald distribution) is a positive-support model that is
#' right-skewed and often used for waiting times. This package provides NIMBLE-compatible density,
#' CDF, and RNG functions under the \code{mean}/\code{shape} parameterization
#' (mean \eqn{\mu>0}, shape \eqn{\lambda>0}). A standalone mixture quantile function is computed by
#' numerical inversion elsewhere; \code{qinvGauss} inverts the base CDF.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar giving the probability for the quantile.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param mean Numeric scalar mean parameter \eqn{\mu>0}.
#' @param shape Numeric scalar shape parameter \eqn{\lambda>0}.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return \code{dInvGauss} returns a numeric scalar density; \code{pInvGauss} returns a numeric scalar CDF;
#'   \code{rInvGauss} returns one random draw; \code{qinvGauss} returns a numeric quantile.
#'
#' @examples
#' mean <- 2
#' shape <- 5
#'
#' dInvGauss(2.0, mean, shape, log = 0)
#' pInvGauss(2.0, mean, shape, lower.tail = 1, log.p = 0)
#' qInvGauss(0.50, mean, shape)
#' qInvGauss(0.95, mean, shape)
#' replicate(10, rInvGauss(1, mean, shape))

#'
#' @rdname InvGauss
#' @name InvGauss
#' @aliases dInvGauss pInvGauss rInvGauss qinvGauss
#' @importFrom stats pnorm rnorm runif uniroot
NULL

#' @describeIn InvGauss Inverse Gaussian density function
#' @export
dInvGauss <- nimble::nimbleFunction(
  run = function(x = double(0),
                 mean = double(0),
                 shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    # standard IG density
    z <- shape * (x - mean) * (x - mean) / (2.0 * mean * mean * x)
    logdens <- 0.5 * log(shape) - 0.5 * log(2.0 * pi) -
      1.5 * log(x) - z

    if (log == 1L) return(logdens)
    return(exp(logdens))
  }
)

#' @describeIn InvGauss Inverse Gaussian distribution function
#' @export
pInvGauss <- nimble::nimbleFunction(
  run = function(q = double(0),
                 mean = double(0),
                 shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    bad <- 0
    if (q <= 0.0) bad <- 1
    if (mean <= 0.0) bad <- 1
    if (shape <= 0.0) bad <- 1
    if (bad == 1) {
      if (lower.tail == 0L) {
        if (log.p == 1L) return(0.0)
        return(1.0)
      }
      if (log.p == 1L) return(-Inf)
      return(0.0)
    }

    z1 <- sqrt(shape / q) * (q / mean - 1.0)
    z2 <- -sqrt(shape / q) * (q / mean + 1.0)

    cdf <- pnorm(z1, 0.0, 1.0, 1L, 0L) +
      exp(2.0 * shape / mean) * pnorm(z2, 0.0, 1.0, 1L, 0L)

    if (is.nan(cdf)) cdf <- 0.0
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0L) cdf <- 1.0 - cdf
    if (log.p == 1L) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn InvGauss Inverse Gaussian random generation
#' @export
rInvGauss <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 mean = double(0),
                 shape = double(0)) {
    returnType(double(0))
    if (n != 1L) return(0.0)

    v <- rnorm(1, 0.0, 1.0)
    y <- v * v
    x <- mean + (mean * mean * y) / (2.0 * shape) -
      (mean / (2.0 * shape)) * sqrt(4.0 * mean * shape * y + mean * mean * y * y)

    u <- runif(1, 0.0, 1.0)
    if (u <= mean / (mean + x)) return(x)
    return(mean * mean / x)
  }
)

#' @describeIn InvGauss Inverse Gaussian quantile function
#' @export
qInvGauss <- function(p, mean, shape,
                      lower.tail = TRUE, log.p = FALSE,
                      tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- 0; next }
    if (pi >= 1) { out[i] <- Inf; next }
    out[i] <- stats::uniroot(function(q) pInvGauss(q, mean, shape) - pi,
                             interval = c(0, 1e20),
                             tol = tol, maxiter = maxiter)$root
  }
  out
}


# ==========================================================
# 3) Amoroso base kernels
# ==========================================================
# NOTE: You said Amoroso base functions already exist in your Amoroso script.
# We do not rename or redefine them here. This block provides documentation linkage only.

#' Amoroso distribution
#'
#' Base Amoroso distribution functions as implemented in this package.
#' Function names and parameterization follow your existing Amoroso implementation.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param loc Numeric scalar location parameter.
#' @param scale Numeric scalar scale parameter.
#' @param shape1 Numeric scalar first shape parameter.
#' @param shape2 Numeric scalar second shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return Density/CDF/RNG functions return numeric scalars. The quantile function returns a numeric scalar.
#'
#' @examples
#' loc <- 0
#' scale <- 1.5
#' shape1 <- 2
#' shape2 <- 1.2
#'
#' dAmoroso(1.0, loc, scale, shape1, shape2, log = 0)
#' pAmoroso(1.0, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)
#' qAmoroso(0.50, loc, scale, shape1, shape2)
#' qAmoroso(0.95, loc, scale, shape1, shape2)
#' replicate(10, rAmoroso(1, loc, scale, shape1, shape2))

#' @rdname amoroso
#' @name amoroso
#' @aliases dAmoroso pAmoroso rAmoroso qAmoroso
NULL

#' @describeIn amoroso Density Function of Amoroso Distribution
#' @export
dAmoroso <- nimble::nimbleFunction(
  run = function(x = double(0),
                 loc = double(0),
                 scale = double(0),
                 shape1 = double(0),
                 shape2 = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (scale == 0.0) {
      if (log == 1) return(log(eps)) else return(eps)
    }
    if (scale > 0.0) {
      if (x < loc) {
        if (log == 1) return(log(eps)) else return(eps)
      }
    }
    if (scale < 0.0) {
      if (x > loc) {
        if (log == 1) return(log(eps)) else return(eps)
      }
    }

    z <- (x - loc) / scale
    lik <- abs(shape2/scale) * (z^(shape1*shape2 - 1.0)) * exp(-z^shape2) / gamma(shape1)
    if (lik < eps) lik <- eps
    if (log == 1) return(log(lik)) else return(lik)
  }
)

#' @describeIn amoroso Distribution Function of Amoroso Distribution
#' @export
pAmoroso <- nimble::nimbleFunction(
  run = function(q = double(0),
                 loc = double(0),
                 scale = double(0),
                 shape1 = double(0),
                 shape2 = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (scale == 0.0) {
      if (log.p != 0) return(log(eps)) else return(eps)
    }
    if (scale > 0.0) {
      if (q <= loc) {
        cdf <- 0.0
        if (lower.tail == 0) cdf <- 1.0 - cdf
        if (log.p != 0) return(log(max(cdf, eps)))
        return(cdf)
      }
    }
    if (scale < 0.0) {
      if (q >= loc) {
        cdf <- 1.0
        if (lower.tail == 0) cdf <- 1.0 - cdf
        if (log.p != 0) return(log(max(cdf, eps)))
        return(cdf)
      }
    }

    z <- ((q - loc) / scale)^shape2
    cdf <- pgamma(z, shape = shape1, scale = 1.0)
    if (shape2 < 0) cdf <- 1.0 - cdf
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn amoroso Quantile Function of Amoroso Distribution
#' @export
qAmoroso <- function(p, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  if (shape2 < 0) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  if (p <= 0) return(loc)
  if (p >= 1) return(if (scale > 0) Inf else -Inf)
  z <- qgamma(p, shape = shape1, scale = 1.0)
  loc + scale * (z^(1/shape2))
}

#' @describeIn amoroso Sample generating Function of Amoroso Distribution
#' @export
rAmoroso <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 loc = double(0),
                 scale = double(0),
                 shape1 = double(0),
                 shape2 = double(0)) {
    returnType(double(0))
    if (n != 1) return(loc)
    p <- runif(1, 0.0, 1.0)
    # call R quantile via nimbleFunction's R-call boundary isn't allowed; use gamma inverse and algebra
    if (shape2 < 0) p <- 1.0 - p
    z <- qgamma(p, shape = shape1, scale = 1.0, lower.tail = 1, log.p = 0)
    return(loc + scale * (z)^(1.0/shape2))
  }
)


# ==========================================================
# 4) Cauchy base kernels
# ==========================================================

#' Cauchy distribution
#'
#' Base Cauchy distribution functions implemented as nimbleFunctions so they can be used
#' in NIMBLE models. Parameterization uses location and scale (scale > 0).
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param location Numeric scalar location parameter.
#' @param scale Numeric scalar scale parameter; must be positive.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return \code{dCauchy} returns a numeric scalar density; \code{pCauchy} returns a numeric scalar CDF;
#'   \code{rCauchy} returns one random draw; \code{qCauchy} returns a numeric quantile.
#'
#' @examples
#' location <- 0
#' scale <- 1.5
#'
#' dCauchy(0.5, location, scale, log = 0)
#' pCauchy(0.5, location, scale, lower.tail = 1, log.p = 0)
#' qCauchy(0.50, location, scale)
#' qCauchy(0.95, location, scale)
#' replicate(10, rCauchy(1, location, scale))
#'
#' @rdname cauchy
#' @name cauchy
#' @aliases dCauchy pCauchy rCauchy qCauchy
#' @importFrom stats runif
NULL

#' @describeIn cauchy Cauchy density function
#' @export
dCauchy <- nimble::nimbleFunction(
  run = function(x = double(0),
                 location = double(0),
                 scale = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (scale <= 0.0) {
      if (log == 1) return(log(eps)) else return(eps)
    }
    z <- (x - location) / scale
    val <- 1.0 / (pi * scale * (1.0 + z * z))
    if (val < eps) val <- eps
    if (log == 1) return(log(val))
    return(val)
  }
)

#' @describeIn cauchy Cauchy distribution function
#' @export
pCauchy <- nimble::nimbleFunction(
  run = function(q = double(0),
                 location = double(0),
                 scale = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (scale <= 0.0) {
      if (log.p != 0) return(log(eps)) else return(eps)
    }
    z <- (q - location) / scale
    cdf <- 0.5 + atan(z) / pi
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) return(log(max(cdf, eps)))
    return(cdf)
  }
)

#' @describeIn cauchy Cauchy random generation
#' @export
rCauchy <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 location = double(0),
                 scale = double(0)) {
    returnType(double(0))
    if (n != 1) return(location)
    if (scale <= 0.0) return(location)
    u <- runif(1, 0.0, 1.0)
    return(location + scale * tan(pi * (u - 0.5)))
  }
)

#' @describeIn cauchy Cauchy quantile function
#' @export
qCauchy <- function(p, location, scale,
                    lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  if (p <= 0) return(-Inf)
  if (p >= 1) return(Inf)
  location + scale * tan(pi * (p - 0.5))
}

