#' Inverse Gaussian mixture distribution
#'
#' A finite mixture of inverse Gaussian components provides a flexible bulk model for positive data
#' with right skewness. Each component is parameterized by its mean \code{mean[j]} and shape
#' \code{shape[j]}.
#'
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s for use inside NIMBLE models.
#' The quantile function is an R function computed by numerical inversion of the mixture CDF.
#' The \code{d*}, \code{p*}, and \code{q*} functions accept vector inputs for their first argument
#' and evaluate elementwise; \code{r*} supports \code{n > 1}.
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
#' @return Density/CDF/RNG functions return numeric scalars. \code{qInvGaussMix} returns a numeric vector
#'   with the same length as \code{p}.
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

    out[i] <- stats::uniroot(function(q) as.numeric(pInvGaussMix(q, w, mean, shape, 1, 0)) - pi,
                             interval = c(0, hi),
                             tol = tol, maxiter = maxiter)$root
  }
  out
}


#' Inverse Gaussian mixture with a Gpd tail
#'
#' This family splices a generalized Pareto distribution (Gpd) above a threshold
#' \code{threshold} onto an inverse Gaussian mixture bulk. The bulk probability at the
#' threshold, \eqn{F_{mix}(threshold)}, is used to scale the tail so that the overall
#' CDF remains proper.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param mean,shape Numeric vectors of length \eqn{K} giving component means and shapes.
#' @param threshold Numeric scalar threshold at which the Gpd tail is attached.
#' @param tail_scale Numeric scalar Gpd scale parameter; must be positive.
#' @param tail_shape Numeric scalar Gpd shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#' @param tol Numeric tolerance for numerical inversion in \code{qInvGaussGpd}.
#' @param maxiter Maximum iterations for numerical inversion in \code{qInvGaussGpd}.
#' @param tol Numeric tolerance for numerical inversion in \code{qInvGaussGpd}.
#' @param maxiter Maximum iterations for numerical inversion in \code{qInvGaussGpd}.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars.
#'   \code{qInvGaussMixGpd} returns a numeric vector with the same length as \code{p}.
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


#' @describeIn InvGauss_mixgpd Inverse Gaussian mixture + Gpd tail density
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


#' @describeIn InvGauss_mixgpd Inverse Gaussian mixture + Gpd tail distribution function
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


#' @describeIn InvGauss_mixgpd Inverse Gaussian mixture + Gpd tail random generation
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


#' @describeIn InvGauss_mixgpd Inverse Gaussian mixture + Gpd tail quantile function
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



#' Inverse Gaussian with a Gpd tail
#'
#' Splices a generalized Pareto distribution (Gpd) above \code{threshold} onto a
#' single inverse Gaussian bulk with parameters \code{mean} and \code{shape}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param mean Numeric scalar mean parameter \eqn{\mu>0}.
#' @param shape Numeric scalar shape parameter \eqn{\lambda>0}.
#' @param threshold Numeric scalar threshold at which the Gpd tail is attached.
#' @param tail_scale Numeric scalar Gpd scale parameter; must be positive.
#' @param tail_shape Numeric scalar Gpd shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars.
#'   \code{qInvGaussGpd} returns a numeric vector with the same length as \code{p}.
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


#' @describeIn InvGauss_gpd Inverse Gaussian + Gpd tail density
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


#' @describeIn InvGauss_gpd Inverse Gaussian + Gpd tail distribution function
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


#' @describeIn InvGauss_gpd Inverse Gaussian + Gpd tail random generation
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


#' @describeIn InvGauss_gpd Inverse Gaussian + Gpd tail quantile function
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




