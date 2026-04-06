#' Inverse Gaussian mixture distribution
#'
#' Finite mixture of inverse Gaussian components for positive-support bulk modeling. Each component
#' is parameterized by `mean[j]` and `shape[j]`.
#'
#' The scalar functions in this topic are the compiled building blocks for inverse-Gaussian bulk
#' kernels. For vectorized R usage, use [invgauss_lowercase()].
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w}
#'   internally when needed.
#' @param mean,shape Numeric vectors of length \eqn{K} giving component means and shapes.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Density/CDF/RNG functions return numeric scalars. `qInvGaussMix()` returns a numeric
#'   vector with the same length as `p`.
#'
#' @details
#' The mixture distribution is
#' \deqn{
#' F(x) = \sum_{k=1}^K \tilde{w}_k F_{IG}(x \mid \mu_k,\lambda_k),
#' }
#' where each inverse Gaussian component has mean \eqn{\mu_k} and variance \eqn{\mu_k^3/\lambda_k}.
#' Random generation selects a component using the normalized weights and then generates from the
#' corresponding inverse Gaussian law. Quantiles are computed numerically because the finite-mixture
#' inverse CDF is not available in closed form.
#'
#' The analytical mixture mean is
#' \deqn{
#' E(X) = \sum_{k=1}^K \tilde{w}_k \mu_k.
#' }
#' That expression is used by the package whenever inverse-Gaussian mixtures contribute to
#' posterior predictive means.
#'
#' @seealso [InvGauss_mixgpd()], [InvGauss_gpd()], [invgauss_lowercase()],
#'   [build_nimble_bundle()], [kernel_support_table()].
#' @family inverse-gaussian kernel families
#'
#' @examples
#' w <- c(0.55, 0.30, 0.15)
#' mean <- c(1.0, 2.5, 5.0)
#' shape <- c(2, 4, 8)
#'
#' dInvGaussMix(2.0, w = w, mean = mean, shape = shape, log = 0)
#' pInvGaussMix(2.0, w = w, mean = mean, shape = shape,
#'             lower.tail = 1, log.p = 0)
#' qInvGaussMix(0.50, w = w, mean = mean, shape = shape)
#' qInvGaussMix(0.95, w = w, mean = mean, shape = shape)
#' replicate(10, rInvGaussMix(1, w = w, mean = mean, shape = shape))
#' @rdname InvGauss_mix
#' @name InvGauss_mix
#' @aliases dInvGaussMix pInvGaussMix rInvGaussMix qInvGaussMix
#' @importFrom stats uniroot
NULL

#' @describeIn InvGauss_mix Inverse Gaussian mixture density
#' @export
dInvGaussMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    eps <- 1e-300
    K <- length(w)

    # wsum = sum(w)
    wsum <- 0.0
    for (j in 1:K) {
      wsum <- wsum + w[j]
    }

    if (wsum <= 0.0) {
      if (log == 1L) return(log(eps)) else return(0.0)
    }

    s0 <- 0.0
    for (j in 1:K) {
      s0 <- s0 + (w[j] / wsum) * dInvGauss(x, mean[j], shape[j], 0L)
    }

    if (s0 < eps) s0 <- eps
    if (log == 1L) return(log(s0)) else return(s0)
  }
)


#' @describeIn InvGauss_mix Inverse Gaussian mixture distribution function
#' @export
pInvGaussMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    eps <- 1e-300
    K <- length(w)

    # wsum = sum(w)
    wsum <- 0.0
    for (j in 1:K) {
      wsum <- wsum + w[j]
    }

    if (wsum <= 0.0) {
      if (log.p != 0L) return(log(eps)) else return(eps)
    }

    cdf <- 0.0
    for (j in 1:K) {
      cdf <- cdf + (w[j] / wsum) * pInvGauss(q, mean[j], shape[j], 1L, 0L)
    }

    # clamp to [0,1]
    if (is.nan(cdf)) cdf <- 0.0
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0L) cdf <- 1.0 - cdf

    if (log.p != 0L) {
      if (cdf < eps) cdf <- eps
      return(log(cdf))
    }

    return(cdf)
  }
)


#' @describeIn InvGauss_mix Inverse Gaussian mixture random generation
#' @export
rInvGaussMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1)) {
    returnType(double(0))

    if (n != 1L) return(0.0)

    K <- length(w)

    # wsum = sum(w)
    wsum <- 0.0
    for (j in 1:K) {
      wsum <- wsum + w[j]
    }
    if (wsum <= 0.0) return(0.0)

    u <- runif(1, 0.0, wsum)

    cw <- 0.0
    idx <- 1
    found <- 0L

    for (j in 1:K) {
      cw <- cw + w[j]
      if (found == 0L) {
        if (u <= cw) {
          idx <- j
          found <- 1L
        }
      }
    }

    return(rInvGauss(1L, mean[idx], shape[idx]))
  }
)


#' @describeIn InvGauss_mix Inverse Gaussian mixture quantile function
#' @export
qInvGaussMix <- function(p, w, mean, shape,
                         lower.tail = TRUE, log.p = FALSE,
                         tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w)
  wsum <- sum(w)
  if (!is.finite(wsum) || wsum <= 0) return(rep(0, length(p)))
  w <- w / wsum
  mean_mix <- sum(w * mean)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- 0; next }
    if (pi >= 1) { out[i] <- Inf; next }
    p0 <- as.numeric(pInvGaussMix(0, w, mean, shape, 1, 0))
    if (!is.finite(p0)) p0 <- 0
    if (p0 >= pi) { out[i] <- 0; next }

    hi <- max(1, mean_mix * 10)
    phi <- as.numeric(pInvGaussMix(hi, w, mean, shape, 1, 0))
    iter <- 0L
    while (is.finite(phi) && phi < pi && hi < 1e20 && iter < 60L) {
      hi <- hi * 2
      phi <- as.numeric(pInvGaussMix(hi, w, mean, shape, 1, 0))
      iter <- iter + 1L
    }

    if (!is.finite(phi) || phi < pi) { out[i] <- Inf; next }

    f0 <- as.numeric(pInvGaussMix(0, w, mean, shape, 1, 0) - pi)
    fhi <- as.numeric(pInvGaussMix(hi, w, mean, shape, 1, 0) - pi)
    if (!is.finite(fhi) || f0 * fhi > 0) {
      out[i] <- Inf
    } else {
      out[i] <- stats::uniroot(function(q) as.numeric(pInvGaussMix(q, w, mean, shape, 1, 0)) - pi,
                               interval = c(0, hi),
                               tol = tol, maxiter = maxiter)$root
    }
  }
  out
}

meanInvGaussMix <- function(w, mean, shape) {
  ww <- .mean_norm_mix_weights(w)
  if (is.null(ww)) return(NA_real_)
  mu <- as.numeric(mean)
  lam <- as.numeric(shape)
  if (length(mu) != length(ww) || length(lam) != length(ww)) return(NA_real_)
  if (any(!is.finite(mu)) || any(!is.finite(lam)) || any(mu <= 0) || any(lam <= 0)) return(NA_real_)
  sum(ww * mu)
}

meanInvGaussMixTrunc <- function(w, mean, shape, threshold) {
  ww <- .mean_norm_mix_weights(w)
  if (is.null(ww)) return(NA_real_)
  mu <- as.numeric(mean)
  lam <- as.numeric(shape)
  u <- as.numeric(threshold)[1]
  if (length(mu) != length(ww) || length(lam) != length(ww)) return(NA_real_)
  if (any(!is.finite(mu)) || any(!is.finite(lam)) || any(mu <= 0) || any(lam <= 0) || is.na(u)) return(NA_real_)
  if (is.infinite(u) && u > 0) return(meanInvGaussMix(w = ww, mean = mu, shape = lam))
  if (u <= 0) return(0)
  sqrt_term <- sqrt(lam / u)
  a <- sqrt_term * (u / mu - 1)
  b <- -sqrt_term * (u / mu + 1)
  comp <- mu * (stats::pnorm(a) - exp(2 * lam / mu) * stats::pnorm(b))
  sum(ww * comp)
}


#' Inverse Gaussian mixture with a GPD tail
#'
#' Spliced bulk-tail family formed by attaching a generalized Pareto tail to an inverse Gaussian
#' mixture bulk.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param mean,shape Numeric vectors of length \eqn{K} giving component means and shapes.
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars.
#'   `qInvGaussMixGpd()` returns a numeric vector with the same length as `p`.
#'
#' @details
#' This family keeps the inverse-Gaussian mixture body below the threshold \eqn{u} and attaches a
#' generalized Pareto exceedance law to the residual survival probability above \eqn{u}. If
#' \eqn{F_{mix}(u)=p_u}, then the tail density is \eqn{(1-p_u)g_{GPD}(x \mid u,\sigma_u,\xi)}.
#'
#' Quantile evaluation is piecewise. For probabilities at or below \eqn{p_u}, the function solves
#' the mixture inverse numerically; above \eqn{p_u}, it rescales the upper-tail probability and
#' applies the GPD inverse directly.
#'
#' @seealso [InvGauss_mix()], [InvGauss_gpd()], [gpd()], [invgauss_lowercase()], [dpmgpd()].
#' @family inverse-gaussian kernel families
#'
#' @examples
#' w <- c(0.55, 0.30, 0.15)
#' mean <- c(1.0, 2.5, 5.0)
#' shape <- c(2, 4, 8)
#' threshold <- 3
#' tail_scale <- 0.9
#' tail_shape <- 0.2
#'
#' dInvGaussMixGpd(4.0, w = w, mean = mean, shape = shape,
#'                threshold = threshold, tail_scale = tail_scale,
#'                tail_shape = tail_shape, log = 0)
#' pInvGaussMixGpd(4.0, w = w, mean = mean, shape = shape,
#'                threshold = threshold, tail_scale = tail_scale,
#'                tail_shape = tail_shape, lower.tail = 1, log.p = 0)
#' qInvGaussMixGpd(0.50, w = w, mean = mean, shape = shape,
#'                threshold = threshold, tail_scale = tail_scale,
#'                tail_shape = tail_shape)
#' qInvGaussMixGpd(0.95, w = w, mean = mean, shape = shape,
#'                threshold = threshold, tail_scale = tail_scale,
#'                tail_shape = tail_shape)
#' replicate(10, rInvGaussMixGpd(1, w = w, mean = mean, shape = shape,
#'                              threshold = threshold,
#'                              tail_scale = tail_scale,
#'                              tail_shape = tail_shape))
#' @rdname InvGauss_mixgpd
#' @name InvGauss_mixgpd
#' @aliases dInvGaussMixGpd pInvGaussMixGpd rInvGaussMixGpd qInvGaussMixGpd
NULL


#' @describeIn InvGauss_mixgpd Inverse Gaussian mixture + GPD tail density
#' @export
dInvGaussMixGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (x < threshold) return(dInvGaussMix(x, w, mean, shape, log))

    Fu <- pInvGaussMix(threshold, w, mean, shape, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)


#' @describeIn InvGauss_mixgpd Inverse Gaussian mixture + GPD tail distribution function
#' @export
pInvGaussMixGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (q < threshold) return(pInvGaussMix(q, w, mean, shape, lower.tail, log.p))

    Fu <- pInvGaussMix(threshold, w, mean, shape, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    G <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    cdf <- max(min(cdf, 1.0), 0.0)

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)


#' @describeIn InvGauss_mixgpd Inverse Gaussian mixture + GPD tail random generation
#' @export
rInvGaussMixGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pInvGaussMix(threshold, w, mean, shape, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rInvGaussMix(1, w, mean, shape))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)


#' @describeIn InvGauss_mixgpd Inverse Gaussian mixture + GPD tail quantile function
#' @export
qInvGaussMixGpd <- function(p, w, mean, shape, threshold, tail_scale, tail_shape,
                            lower.tail = TRUE, log.p = FALSE,
                            tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)

  Fu <- as.numeric(pInvGaussMix(threshold, w, mean, shape, 1, 0))
  Fu <- max(min(Fu, 1.0), 0.0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- qInvGaussMix(pi, w, mean, shape,
                             lower.tail = TRUE, log.p = FALSE,
                             tol = tol, maxiter = maxiter)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}



#' Inverse Gaussian with a GPD tail
#'
#' Spliced family obtained by attaching a generalized Pareto tail above `threshold` to a single
#' inverse Gaussian bulk.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param mean Numeric scalar mean parameter \eqn{\mu>0}.
#' @param shape Numeric scalar shape parameter \eqn{\lambda>0}.
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars.
#'   `qInvGaussGpd()` returns a numeric vector with the same length as `p`.
#'
#' @details
#' This is the one-component version of [InvGauss_mixgpd()]. The inverse Gaussian governs the bulk
#' region and the generalized Pareto governs exceedances over the threshold. The splice is
#' continuous at \eqn{u} because the GPD is scaled by the inverse-Gaussian survival probability at
#' the threshold.
#'
#' The ordinary mean of the spliced law exists only when the GPD tail has \eqn{\xi < 1}. When that
#' condition fails, the package uses restricted means or quantile-based summaries instead of an
#' ordinary mean.
#'
#' @seealso [InvGauss_mix()], [InvGauss_mixgpd()], [gpd()], [invgauss_lowercase()].
#' @family inverse-gaussian kernel families
#'
#' @examples
#' mean <- 2.5
#' shape <- 6
#' threshold <- 3
#' tail_scale <- 0.9
#' tail_shape <- 0.2
#'
#' dInvGaussGpd(4.0, mean = mean, shape = shape,
#'             threshold = threshold, tail_scale = tail_scale,
#'             tail_shape = tail_shape, log = 0)
#' pInvGaussGpd(4.0, mean = mean, shape = shape,
#'             threshold = threshold, tail_scale = tail_scale,
#'             tail_shape = tail_shape, lower.tail = 1, log.p = 0)
#' qInvGaussGpd(0.50, mean = mean, shape = shape,
#'             threshold = threshold, tail_scale = tail_scale,
#'             tail_shape = tail_shape)
#' qInvGaussGpd(0.95, mean = mean, shape = shape,
#'             threshold = threshold, tail_scale = tail_scale,
#'             tail_shape = tail_shape)
#' replicate(10, rInvGaussGpd(1, mean = mean, shape = shape,
#'                           threshold = threshold,
#'                           tail_scale = tail_scale,
#'                           tail_shape = tail_shape))
#' @rdname InvGauss_gpd
#' @name InvGauss_gpd
#' @aliases dInvGaussGpd pInvGaussGpd rInvGaussGpd qInvGaussGpd
NULL


#' @describeIn InvGauss_gpd Inverse Gaussian + GPD tail density
#' @export
dInvGaussGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 mean = double(0),
                 shape = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (x < threshold) return(dInvGauss(x, mean, shape, log))

    Fu <- pInvGauss(threshold, mean, shape, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)


#' @describeIn InvGauss_gpd Inverse Gaussian + GPD tail distribution function
#' @export
pInvGaussGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 mean = double(0),
                 shape = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (q < threshold) return(pInvGauss(q, mean, shape, lower.tail, log.p))

    Fu <- pInvGauss(threshold, mean, shape, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    G <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    cdf <- max(min(cdf, 1.0), 0.0)

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)


#' @describeIn InvGauss_gpd Inverse Gaussian + GPD tail random generation
#' @export
rInvGaussGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 mean = double(0),
                 shape = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pInvGauss(threshold, mean, shape, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rInvGauss(1, mean, shape))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)


#' @describeIn InvGauss_gpd Inverse Gaussian + GPD tail quantile function
#' @export
#' @param tol Numeric tolerance for numerical inversion in \code{qInvGaussGpd}.
#' @param maxiter Maximum iterations for numerical inversion in \code{qInvGaussGpd}.
qInvGaussGpd <- function(p, mean, shape, threshold, tail_scale, tail_shape,
                         lower.tail = TRUE, log.p = FALSE,
                         tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- as.numeric(pInvGauss(threshold, mean, shape, 1, 0))
  Fu <- max(min(Fu, 1.0), 0.0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- qInvGauss(pi, mean = mean, shape = shape,
                          lower.tail = TRUE, log.p = FALSE,
                          tol = tol, maxiter = maxiter)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}


# ==========================================================
# Lowercase vectorized R wrappers for Inverse Gaussian kernels
# ==========================================================

#' Lowercase vectorized inverse Gaussian distribution functions
#'
#' Vectorized R wrappers for the scalar inverse-Gaussian-kernel topics in this file.
#'
#' @param x Numeric vector of quantiles.
#' @param q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Integer number of observations to generate.
#' @param w Numeric vector of mixture weights.
#' @param mean,shape Numeric vectors (mix) or scalars (base+gpd) of component parameters.
#' @param threshold,tail_scale,tail_shape GPD tail parameters (scalars).
#' @param log Logical; if \code{TRUE}, return log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le x)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are on log scale.
#' @param tol,maxiter Tolerance and max iterations for numerical inversion.
#'
#' @return Numeric vector of densities, probabilities, quantiles, or random variates.
#'
#' @details
#' These functions are vectorized R front ends to the scalar inverse-Gaussian and splice routines.
#' They retain the \eqn{(\mu,\lambda)} parameterization used everywhere else in the package and
#' simply apply the scalar evaluator repeatedly over the supplied input vector or draw index.
#'
#' @seealso [InvGauss_mix()], [InvGauss_mixgpd()], [InvGauss_gpd()], [bundle()],
#'   [get_kernel_registry()].
#' @family vectorized kernel helpers
#'
#' @examples
#' w <- c(0.6, 0.3, 0.1)
#' mu <- c(1, 1.5, 2)
#' lam <- c(2, 3, 4)
#'
#' # Inverse Gaussian mixture
#' dinvgaussmix(c(1, 2, 3), w = w, mean = mu, shape = lam)
#' rinvgaussmix(5, w = w, mean = mu, shape = lam)
#'
#' @name invgauss_lowercase
#' @rdname invgauss_lowercase
NULL

# ---- InvGauss Mix lowercase wrappers ----

#' @describeIn invgauss_lowercase Inverse Gaussian mixture density (vectorized)
#' @export
dinvgaussmix <- function(x, w, mean, shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dInvGaussMix(xi, w = w, mean = mean, shape = shape, log = log_int)),
         numeric(1L))
}

#' @describeIn invgauss_lowercase Inverse Gaussian mixture distribution function (vectorized)
#' @export
pinvgaussmix <- function(q, w, mean, shape, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pInvGaussMix(qi, w = w, mean = mean, shape = shape,
                                                  lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn invgauss_lowercase Inverse Gaussian mixture quantile function (vectorized)
#' @export
qinvgaussmix <- function(p, w, mean, shape, lower.tail = TRUE, log.p = FALSE,
                         tol = 1e-10, maxiter = 200) {
  qInvGaussMix(p, w = w, mean = mean, shape = shape, lower.tail = lower.tail,
               log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn invgauss_lowercase Inverse Gaussian mixture random generation (vectorized)
#' @export
rinvgaussmix <- function(n, w, mean, shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rInvGaussMix(1L, w = w, mean = mean, shape = shape)),
         numeric(1L))
}

# ---- InvGauss Mix + GPD lowercase wrappers ----

#' @describeIn invgauss_lowercase Inverse Gaussian mixture + GPD density (vectorized)
#' @export
dinvgaussmixgpd <- function(x, w, mean, shape, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dInvGaussMixGpd(xi, w = w, mean = mean, shape = shape,
                                                     threshold = threshold, tail_scale = tail_scale,
                                                     tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn invgauss_lowercase Inverse Gaussian mixture + GPD distribution function (vectorized)
#' @export
pinvgaussmixgpd <- function(q, w, mean, shape, threshold, tail_scale, tail_shape,
                            lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pInvGaussMixGpd(qi, w = w, mean = mean, shape = shape,
                                                     threshold = threshold, tail_scale = tail_scale,
                                                     tail_shape = tail_shape,
                                                     lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn invgauss_lowercase Inverse Gaussian mixture + GPD quantile function (vectorized)
#' @export
qinvgaussmixgpd <- function(p, w, mean, shape, threshold, tail_scale, tail_shape,
                            lower.tail = TRUE, log.p = FALSE, tol = 1e-10, maxiter = 200) {
  qInvGaussMixGpd(p, w = w, mean = mean, shape = shape, threshold = threshold,
                  tail_scale = tail_scale, tail_shape = tail_shape,
                  lower.tail = lower.tail, log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn invgauss_lowercase Inverse Gaussian mixture + GPD random generation (vectorized)
#' @export
rinvgaussmixgpd <- function(n, w, mean, shape, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rInvGaussMixGpd(1L, w = w, mean = mean, shape = shape,
                                                            threshold = threshold, tail_scale = tail_scale,
                                                            tail_shape = tail_shape)),
         numeric(1L))
}

# ---- InvGauss + GPD lowercase wrappers ----

#' @describeIn invgauss_lowercase Inverse Gaussian + GPD density (vectorized)
#' @export
dinvgaussgpd <- function(x, mean, shape, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dInvGaussGpd(xi, mean = mean, shape = shape,
                                                  threshold = threshold, tail_scale = tail_scale,
                                                  tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn invgauss_lowercase Inverse Gaussian + GPD distribution function (vectorized)
#' @export
pinvgaussgpd <- function(q, mean, shape, threshold, tail_scale, tail_shape,
                         lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pInvGaussGpd(qi, mean = mean, shape = shape,
                                                  threshold = threshold, tail_scale = tail_scale,
                                                  tail_shape = tail_shape,
                                                  lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn invgauss_lowercase Inverse Gaussian + GPD quantile function (vectorized)
#' @export
qinvgaussgpd <- function(p, mean, shape, threshold, tail_scale, tail_shape,
                         lower.tail = TRUE, log.p = FALSE, tol = 1e-10, maxiter = 200) {
  qInvGaussGpd(p, mean = mean, shape = shape, threshold = threshold,
               tail_scale = tail_scale, tail_shape = tail_shape,
               lower.tail = lower.tail, log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn invgauss_lowercase Inverse Gaussian + GPD random generation (vectorized)
#' @export
rinvgaussgpd <- function(n, mean, shape, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rInvGaussGpd(1L, mean = mean, shape = shape,
                                                          threshold = threshold, tail_scale = tail_scale,
                                                          tail_shape = tail_shape)),
         numeric(1L))
}

