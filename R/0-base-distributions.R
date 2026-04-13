# ==========================================================
# 1) Generalized Pareto distribution (GPD)
# ==========================================================

.mean_norm_mix_weights <- function(w) {
  w <- as.numeric(w)
  if (!length(w) || any(!is.finite(w))) return(NULL)
  wsum <- sum(w)
  if (!is.finite(wsum) || wsum <= 0) return(NULL)
  w / wsum
}

#' Generalized Pareto distribution
#'
#' Scalar generalized Pareto distribution (GPD) utilities for threshold exceedances above
#' `threshold`. These NIMBLE-compatible functions provide the tail component used by the spliced
#' bulk-tail families elsewhere in the package.
#'
#' The parameterization is
#' \deqn{
#' G(x) = 1 - \left(1 + \xi \frac{x - u}{\sigma_u}\right)^{-1 / \xi}, \qquad x \ge u,
#' }
#' where `threshold = u`, `scale = sigma_u > 0`, and `shape = xi`. When `shape` approaches zero,
#' the distribution reduces to the exponential tail limit.
#'
#' These uppercase NIMBLE-compatible functions are scalar (`x`/`q` and `n = 1`).
#' For vectorized R usage, use [base_lowercase()].
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
#' @return `dGpd()` returns a numeric scalar density, `pGpd()` returns a numeric scalar CDF,
#'   `rGpd()` returns one random draw, and `qGpd()` returns a numeric quantile.
#'
#' @details
#' The associated density is
#' \deqn{
#' g(x) = \frac{1}{\sigma_u}
#' \left(1 + \xi \frac{x - u}{\sigma_u}\right)^{-1/\xi - 1},
#' }
#' on the support where \eqn{1 + \xi (x-u)/\sigma_u > 0}. When \eqn{\xi = 0}, this reduces to the
#' exponential density with mean excess \eqn{\sigma_u}. The GPD has finite mean only when
#' \eqn{\xi < 1}, and finite variance only when \eqn{\xi < 1/2}. Those existence conditions matter
#' for downstream predictive means in the package: spliced models with \eqn{\xi \ge 1} require
#' restricted means rather than ordinary means.
#'
#' If a bulk distribution has CDF \eqn{F_{bulk}}, the package's spliced families use the tail construction
#' \deqn{
#' F(x) =
#' \left\{
#' \begin{array}{ll}
#' F_{bulk}(x), & x < u, \\
#' F_{bulk}(u) + \{1 - F_{bulk}(u)\} G(x), & x \ge u.
#' \end{array}
#' \right.
#' }
#'
#' @seealso [base_lowercase()], [normal_gpd()], [lognormal_gpd()], [gamma_gpd()],
#'   [InvGauss_gpd()], [laplace_gpd()], [amoroso_gpd()].
#' @family base tail distributions
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
      if (is.nan(cdf)) cdf <- 0.0
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
    if (pi <= 0) {
      out[i] <- threshold
      next
    }
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
#' Scalar inverse Gaussian utilities under the \eqn{(\mu, \lambda)} parameterization, where
#' `mean = mu > 0` and `shape = lambda > 0`. These functions are used directly and as building
#' blocks for inverse-Gaussian mixtures and spliced inverse-Gaussian-plus-GPD families.
#'
#' The density is
#' \deqn{
#' f(x) = \left(\frac{\lambda}{2 \pi x^3}\right)^{1/2}
#' \exp\left\{- \frac{\lambda (x - \mu)^2}{2 \mu^2 x}\right\}, \qquad x > 0.
#' }
#'
#' These uppercase NIMBLE-compatible functions are scalar (`x`/`q` and `n = 1`).
#' For vectorized R usage, use [base_lowercase()].
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
#' @return `dInvGauss()` returns a numeric scalar density, `pInvGauss()` returns a numeric scalar
#'   CDF, `rInvGauss()` returns one random draw, and `qInvGauss()` returns a numeric quantile.
#'
#' @details
#' The inverse Gaussian is the first-passage-time distribution of a Brownian motion with positive
#' drift. Under the \eqn{(\mu, \lambda)} parameterization used here, the mean is \eqn{E(X)=\mu} and
#' the variance is \eqn{\mathrm{Var}(X)=\mu^3/\lambda}. The implementation follows that
#' parameterization throughout the package, so inverse-Gaussian mixture and splice families inherit
#' the same interpretation.
#'
#' The distribution function is evaluated through the standard normal representation
#' \deqn{
#' F(x) =
#' \Phi\left(\sqrt{\frac{\lambda}{x}}\left(\frac{x}{\mu}-1\right)\right) +
#' \exp\left(\frac{2\lambda}{\mu}\right)
#' \Phi\left(-\sqrt{\frac{\lambda}{x}}\left(\frac{x}{\mu}+1\right)\right),
#' }
#' and the quantile is obtained numerically because no simple closed form is available.
#'
#' @seealso [InvGauss_mix()], [InvGauss_gpd()], [base_lowercase()], [build_nimble_bundle()].
#' @family base bulk distributions
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
#' @aliases dInvGauss pInvGauss rInvGauss qInvGauss
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
    if (pi <= 0) {
      out[i] <- 0
      next
    }
    if (pi >= 1) {
      out[i] <- Inf
      next
    }
    hi <- max(1, mean * 10)
    f0 <- as.numeric(pInvGauss(0, mean, shape) - pi)
    fhi <- as.numeric(pInvGauss(hi, mean, shape) - pi)
    iter <- 0L
    while (is.finite(fhi) && f0 * fhi > 0 && hi < 1e20 && iter < 60L) {
      hi <- hi * 2
      fhi <- as.numeric(pInvGauss(hi, mean, shape) - pi)
      iter <- iter + 1L
    }
    if (!is.finite(fhi) || f0 * fhi > 0) {
      out[i] <- Inf
    } else {
      out[i] <- stats::uniroot(function(q) pInvGauss(q, mean, shape) - pi,
                               interval = c(0, hi),
                               tol = tol, maxiter = maxiter)$root
    }
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
#' Scalar Amoroso utilities used as flexible positive-support base kernels and mixture components.
#' The package parameterization uses `loc`, `scale`, `shape1`, and `shape2`, allowing the family
#' to represent a broad range of skewed bulk shapes.
#'
#' Writing `loc = a`, `scale = theta`, `shape1 = alpha`, and `shape2 = beta`, the transformed
#' quantity \eqn{((X - a) / \theta)^\beta} follows a gamma law with shape \eqn{\alpha}.
#'
#' These uppercase NIMBLE-compatible functions are scalar (`x`/`q` and `n = 1`).
#' For vectorized R usage, use [base_lowercase()].
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
#' @return Density/CDF/RNG functions return numeric scalars. The quantile function returns a numeric
#'   scalar or vector matching the length of `p`.
#'
#' @details
#' The Amoroso family used in the package is defined by the density
#' \deqn{
#' f(x) =
#' \left|\frac{\beta}{\theta}\right|
#' \frac{z^{\alpha \beta - 1}\exp(-z^\beta)}{\Gamma(\alpha)},
#' \qquad
#' z = \frac{x-a}{\theta},
#' }
#' on the side of the location parameter determined by the sign of \eqn{\theta}. Equivalently,
#' \eqn{Z = ((X-a)/\theta)^\beta} follows a Gamma distribution with shape \eqn{\alpha} and unit
#' scale. That representation explains why the quantile function is computed from a gamma quantile
#' and then mapped back through the inverse transformation.
#'
#' The mean exists whenever \eqn{\alpha + 1/\beta} lies in the domain of the gamma function used by
#' the moment formula. In the package this family serves as a flexible positive-support bulk kernel
#' capable of reproducing gamma-like, Weibull-like, and other skewed shapes with a single
#' parameterization.
#'
#' @seealso [amoroso_mix()], [amoroso_gpd()], [base_lowercase()], [kernel_support_table()].
#' @family base bulk distributions
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
    lik <- abs(shape2 / scale) * (z^(shape1 * shape2 - 1.0)) * exp(-z^shape2) / gamma(shape1)
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

  len <- max(length(p), length(loc), length(scale), length(shape1), length(shape2))
  p <- rep_len(p, len)
  loc <- rep_len(loc, len)
  scale <- rep_len(scale, len)
  shape1 <- rep_len(shape1, len)
  shape2 <- rep_len(shape2, len)

  p <- ifelse(shape2 < 0, 1 - p, p)
  p <- pmax(pmin(p, 1), 0)

  out <- numeric(len)
  at_zero <- p <= 0
  at_one <- p >= 1
  out[at_zero] <- loc[at_zero]
  out[at_one] <- ifelse(scale[at_one] > 0, Inf, -Inf)

  mid <- !(at_zero | at_one)
  if (any(mid)) {
    z <- stats::qgamma(p[mid], shape = shape1[mid], scale = 1.0)
    out[mid] <- loc[mid] + scale[mid] * (z^(1 / shape2[mid]))
  }

  out
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
    return(loc + scale * (z)^(1.0 / shape2))
  }
)


# ==========================================================
# 4) Cauchy base kernels
# ==========================================================

#' Cauchy distribution
#'
#' Scalar Cauchy utilities implemented for NIMBLE compatibility. These functions support symmetric
#' heavy-tailed kernels on the real line and feed directly into the finite Cauchy mixture family.
#'
#' The density is
#' \deqn{
#' f(x) = \frac{1}{\pi s \{1 + ((x - \ell)/s)^2\}},
#' }
#' where `location = ell` and `scale = s > 0`.
#'
#' These uppercase NIMBLE-compatible functions are scalar (`x`/`q` and `n = 1`).
#' For vectorized R usage, use [base_lowercase()].
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
#' @return `dCauchy()` returns a numeric scalar density, `pCauchy()` returns a numeric scalar CDF,
#'   `rCauchy()` returns one random draw, and `qCauchy()` returns a numeric quantile.
#'
#' @details
#' The Cauchy law is a stable heavy-tailed distribution with undefined mean and variance. That is
#' why the package allows the Cauchy kernel only as a bulk distribution and deliberately does not
#' pair it with GPD tails in the kernel registry. For predictive summaries,
#' ordinary means are not available under Cauchy kernels; medians, quantiles,
#' survival curves, and restricted means remain well defined.
#'
#' The distribution function is
#' \deqn{
#' F(x) = \frac{1}{2} + \frac{1}{\pi}\arctan\left(\frac{x-\ell}{s}\right),
#' }
#' and the quantile is the corresponding inverse
#' \deqn{
#' Q(p) = \ell + s \tan\{\pi(p-1/2)\}.
#' }
#'
#' @seealso [cauchy_mix()], [base_lowercase()], [kernel_support_table()].
#' @family base bulk distributions
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
    if (is.nan(cdf)) cdf <- 0.0
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
  # Handle boundary cases for vectorized input
  out <- location + scale * tan(pi * (p - 0.5))
  out[p <= 0] <- -Inf
  out[p >= 1] <- Inf
  out
}


# ==========================================================
# 5) Lowercase vectorized R wrappers for base kernels
# ==========================================================

#' Lowercase vectorized distribution functions (base kernels)
#'
#' Vectorized R wrappers around the scalar base-kernel functions defined in this file. These
#' helpers are intended for interactive R use, examples, testing, and checking numerical behavior
#' outside compiled NIMBLE code.
#'
#' The wrappers preserve the same parameterizations as the uppercase scalar functions, but accept
#' vector inputs for `x`, `q`, or `p` and allow `n > 1` for random generation.
#'
#' @param x Numeric vector of quantiles.
#' @param q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Integer number of observations to generate.
#' @param threshold,scale,shape,mean,loc,shape1,shape2,location
#'   Distribution parameters (scalars).
#' @param log Logical; if \code{TRUE}, return log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are
#'   \eqn{P(X \le x)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are on log scale.
#' @param tol,maxiter Tolerance and max iterations for numerical inversion.
#'
#' @return Numeric vector of densities, probabilities, quantiles, or random variates.
#'
#' @details
#' Each lowercase helper is a vectorized R wrapper around the corresponding
#' uppercase scalar routine documented in this file. The wrapper keeps the same
#' parameterization and applies the scalar kernel repeatedly over the supplied
#' evaluation points or simulation index. These helpers are therefore
#' appropriate for interactive analysis, testing, and examples, whereas the
#' uppercase functions are the building blocks used inside NIMBLE model code.
#'
#' The wrappers do not change the underlying theory. For example, \code{qgpd()} still uses the
#' closed-form GPD inverse, \code{qinvgauss()} still performs numerical inversion of the inverse
#' Gaussian CDF, and \code{qamoroso()} still maps a gamma quantile through the Amoroso
#' transformation. Random-generation wrappers call the corresponding scalar RNG repeatedly when
#' \code{n > 1}.
#'
#' @seealso [gpd()], [InvGauss()], [amoroso()], [cauchy()], [build_nimble_bundle()],
#'   [kernel_support_table()].
#' @family vectorized kernel helpers
#'
#' @examples
#' # GPD
#' dgpd(c(1.5, 2.0, 2.5), threshold = 1, scale = 0.8, shape = 0.2)
#' pgpd(c(1.5, 2.0), threshold = 1, scale = 0.8, shape = 0.2)
#' qgpd(c(0.5, 0.9), threshold = 1, scale = 0.8, shape = 0.2)
#' rgpd(5, threshold = 1, scale = 0.8, shape = 0.2)
#'
#' # Inverse Gaussian
#' dinvgauss(c(1, 2, 3), mean = 2, shape = 5)
#' rinvgauss(5, mean = 2, shape = 5)
#'
#' # Amoroso
#' damoroso(c(1, 2), loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2)
#' ramoroso(5, loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2)
#'
#' # Cauchy
#' dcauchy_vec(c(-1, 0, 1), location = 0, scale = 1)
#' rcauchy_vec(5, location = 0, scale = 1)
#'
#' @name base_lowercase
#' @rdname base_lowercase
NULL

# ---- GPD lowercase wrappers ----

#' @describeIn base_lowercase GPD density (vectorized)
#' @export
dgpd <- function(x, threshold, scale, shape, log = FALSE) {

  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dGpd(xi, threshold, scale, shape, log_int)),
         numeric(1L))
}

#' @describeIn base_lowercase GPD distribution function (vectorized)
#' @export
pgpd <- function(q, threshold, scale, shape, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)

  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pGpd(qi, threshold, scale, shape, lt_int, lp_int)),
         numeric(1L))
}

#' @describeIn base_lowercase GPD quantile function (vectorized)
#' @export
qgpd <- function(p, threshold, scale, shape, lower.tail = TRUE, log.p = FALSE) {
  qGpd(p, threshold, scale, shape, lower.tail = lower.tail, log.p = log.p)
}

#' @describeIn base_lowercase GPD random generation (vectorized)
#' @export
rgpd <- function(n, threshold, scale, shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rGpd(1L, threshold, scale, shape)),
         numeric(1L))
}

# ---- Inverse Gaussian lowercase wrappers ----

#' @describeIn base_lowercase Inverse Gaussian density (vectorized)
#' @export
dinvgauss <- function(x, mean, shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dInvGauss(xi, mean, shape, log_int)),
         numeric(1L))
}

#' @describeIn base_lowercase Inverse Gaussian distribution function (vectorized)
#' @export
pinvgauss <- function(q, mean, shape, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pInvGauss(qi, mean, shape, lt_int, lp_int)),
         numeric(1L))
}

#' @describeIn base_lowercase Inverse Gaussian quantile function (vectorized)
#' @export
qinvgauss <- function(p, mean, shape, lower.tail = TRUE, log.p = FALSE,
                      tol = 1e-10, maxiter = 200) {
  qInvGauss(p, mean, shape, lower.tail = lower.tail, log.p = log.p,
            tol = tol, maxiter = maxiter)
}

#' @describeIn base_lowercase Inverse Gaussian random generation (vectorized)
#' @export
rinvgauss <- function(n, mean, shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rInvGauss(1L, mean, shape)),
         numeric(1L))
}

# ---- Amoroso lowercase wrappers ----

#' @describeIn base_lowercase Amoroso density (vectorized)
#' @export
damoroso <- function(x, loc, scale, shape1, shape2, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dAmoroso(xi, loc, scale, shape1, shape2, log_int)),
         numeric(1L))
}

#' @describeIn base_lowercase Amoroso distribution function (vectorized)
#' @export
pamoroso <- function(q, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pAmoroso(qi, loc, scale, shape1, shape2, lt_int, lp_int)),
         numeric(1L))
}

#' @describeIn base_lowercase Amoroso quantile function (vectorized)
#' @export
qamoroso <- function(p, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  qAmoroso(p, loc, scale, shape1, shape2, lower.tail = lower.tail, log.p = log.p)
}

#' @describeIn base_lowercase Amoroso random generation (vectorized)
#' @export
ramoroso <- function(n, loc, scale, shape1, shape2) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rAmoroso(1L, loc, scale, shape1, shape2)),
         numeric(1L))
}

# ---- Cauchy lowercase wrappers ----

#' @describeIn base_lowercase Cauchy density (vectorized)
#' @export
dcauchy_vec <- function(x, location, scale, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dCauchy(xi, location, scale, log_int)),
         numeric(1L))
}

#' @describeIn base_lowercase Cauchy distribution function (vectorized)
#' @export
pcauchy_vec <- function(q, location, scale, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pCauchy(qi, location, scale, lt_int, lp_int)),
         numeric(1L))
}

#' @describeIn base_lowercase Cauchy quantile function (vectorized)
#' @export
qcauchy_vec <- function(p, location, scale, lower.tail = TRUE, log.p = FALSE) {
  qCauchy(p, location, scale, lower.tail = lower.tail, log.p = log.p)
}

#' @describeIn base_lowercase Cauchy random generation (vectorized)
#' @export
rcauchy_vec <- function(n, location, scale) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rCauchy(1L, location, scale)),
         numeric(1L))
}

