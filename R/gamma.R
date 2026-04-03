#' Gamma mixture distribution
#'
#' Finite mixture of gamma components for positive-support bulk modeling. The scalar functions in
#' this topic are the compiled building blocks behind the gamma bulk kernel family.
#'
#' The mixture density is
#' \deqn{
#' f(x) = \sum_{k = 1}^K \tilde{w}_k f_{\Gamma}(x \mid \alpha_k, \theta_k),
#' \qquad x > 0,
#' }
#' with normalized weights \eqn{\tilde{w}_k}. For vectorized R usage, use [gamma_lowercase()].
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w}
#'   internally when needed.
#' @param shape,scale Numeric vectors of length \eqn{K} giving Gamma shapes and rates.
#' @param log Logical; if \code{TRUE}, return the log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Density/CDF/RNG functions return numeric scalars. `qGammaMix()` returns a numeric vector
#'   with the same length as `p`.
#'
#' @seealso [gamma_mixgpd()], [gamma_gpd()], [gamma_lowercase()], [build_nimble_bundle()],
#'   [kernel_support_table()].
#' @family gamma kernel families
#'
#' @examples
#' w <- c(0.55, 0.30, 0.15)
#' scale <- c(1.0, 2.5, 5.0)
#' shape <- c(2, 4, 6)
#'
#' dGammaMix(2.0, w = w, scale = scale, shape = shape, log = 0)
#' pGammaMix(2.0, w = w, scale = scale, shape = shape, lower.tail = 1, log.p = 0)
#' qGammaMix(0.50, w = w, scale = scale, shape = shape)
#' qGammaMix(0.95, w = w, scale = scale, shape = shape)
#' replicate(10, rGammaMix(1, w = w, scale = scale, shape = shape))
#' @rdname gamma_mix
#' @name gamma_mix
#' @aliases dGammaMix pGammaMix rGammaMix qGammaMix
#' @importFrom stats dgamma pgamma rgamma qgamma runif uniroot
NULL

#' @describeIn gamma_mix Gamma mixture density
#' @export
dGammaMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 shape = double(1),
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
      s0 <- s0 + ww[j] * dgamma(x, shape = shape[j], scale = scale[j], log = 0)
    }

    if (log == 1) return(log(s0))
    return(s0)
  }
)

#' @describeIn gamma_mix Gamma mixture distribution function
#' @export
pGammaMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 shape = double(1),
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
      cdf <- cdf + ww[j] * pgamma(q, shape = shape[j], scale = scale[j],
                                  lower.tail = 1, log.p = 0)
    }

    if (is.nan(cdf)) cdf <- 0.0
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn gamma_mix Gamma mixture random generation
#' @export
rGammaMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 shape = double(1),
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

    return(rgamma(1, shape = shape[idx], scale = scale[idx]))
  }
)

# ---- nimbleFunction aliases for internal use ----
dGammaMix_nf <- dGammaMix
pGammaMix_nf <- pGammaMix
rGammaMix_nf <- rGammaMix

#' @describeIn gamma_mix Gamma mixture quantile function
#' @export
qGammaMix <- function(p, w, shape, scale,
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

    hi <- max(stats::qgamma(pi, shape = shape, scale = scale), na.rm = TRUE)
    if (!is.finite(hi) || hi <= 0) hi <- 1
    f0 <- as.numeric(pGammaMix(0, w = w, shape = shape, scale = scale, lower.tail = 1, log.p = 0) - pi)
    fhi <- as.numeric(pGammaMix(hi, w = w, shape = shape, scale = scale, lower.tail = 1, log.p = 0) - pi)
    iter <- 0L
    while (is.finite(fhi) && f0 * fhi > 0 && hi < 1e20 && iter < 60L) {
      hi <- hi * 2
      fhi <- as.numeric(pGammaMix(hi, w = w, shape = shape, scale = scale, lower.tail = 1, log.p = 0) - pi)
      iter <- iter + 1L
    }
    if (!is.finite(hi) || hi <= 0 || !is.finite(fhi) || f0 * fhi > 0) {
      out[i] <- Inf
    } else {
      out[i] <- stats::uniroot(
        function(z) pGammaMix(z, w = w, shape = shape, scale = scale,
                              lower.tail = 1, log.p = 0) - pi,
        interval = c(0, hi),
        tol = tol, maxiter = maxiter
      )$root
    }
  }
  out
}

meanGammaMix <- function(w, shape, scale) {
  ww <- .mean_norm_mix_weights(w)
  if (is.null(ww)) return(NA_real_)
  alpha <- as.numeric(shape)
  theta <- as.numeric(scale)
  if (length(alpha) != length(ww) || length(theta) != length(ww)) return(NA_real_)
  if (any(!is.finite(alpha)) || any(!is.finite(theta)) || any(alpha <= 0) || any(theta <= 0)) return(NA_real_)
  sum(ww * alpha * theta)
}

meanGammaMixTrunc <- function(w, shape, scale, threshold) {
  ww <- .mean_norm_mix_weights(w)
  if (is.null(ww)) return(NA_real_)
  alpha <- as.numeric(shape)
  theta <- as.numeric(scale)
  u <- as.numeric(threshold)[1]
  if (length(alpha) != length(ww) || length(theta) != length(ww)) return(NA_real_)
  if (any(!is.finite(alpha)) || any(!is.finite(theta)) || any(alpha <= 0) || any(theta <= 0) || is.na(u)) return(NA_real_)
  if (is.infinite(u) && u > 0) return(meanGammaMix(w = ww, shape = alpha, scale = theta))
  if (u <= 0) return(0)
  sum(ww * alpha * theta * stats::pgamma(u, shape = alpha + 1, scale = theta))
}

#' Gamma mixture with a GPD tail
#'
#' Spliced bulk-tail family formed by attaching a generalized Pareto tail to a gamma mixture bulk.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param shape,scale Numeric vectors of length \eqn{K} giving Gamma shapes and rates.
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. `qGammaMixGpd()` returns a
#'   numeric vector with the same length as `p`.
#'
#' @seealso [gamma_mix()], [gamma_gpd()], [gpd()], [gamma_lowercase()], [dpmgpd()].
#' @family gamma kernel families
#' @examples
#' w <- c(0.55, 0.30, 0.15)
#' scale <- c(1.0, 2.5, 5.0)
#' shape <- c(2, 4, 6)
#' threshold <- 3
#' tail_scale <- 0.9
#' tail_shape <- 0.2
#'
#' dGammaMixGpd(4.0, w = w, scale = scale, shape = shape,
#'             threshold = threshold, tail_scale = tail_scale,
#'             tail_shape = tail_shape, log = 0)
#' pGammaMixGpd(4.0, w = w, scale = scale, shape = shape,
#'             threshold = threshold, tail_scale = tail_scale,
#'             tail_shape = tail_shape, lower.tail = 1, log.p = 0)
#' qGammaMixGpd(0.50, w = w, scale = scale, shape = shape,
#'             threshold = threshold, tail_scale = tail_scale,
#'             tail_shape = tail_shape)
#' qGammaMixGpd(0.95, w = w, scale = scale, shape = shape,
#'             threshold = threshold, tail_scale = tail_scale,
#'             tail_shape = tail_shape)
#' replicate(10, rGammaMixGpd(1, w = w, scale = scale, shape = shape,
#'                           threshold = threshold,
#'                           tail_scale = tail_scale,
#'                           tail_shape = tail_shape))
#' @rdname gamma_mixgpd
#' @name gamma_mixgpd
#' @aliases dGammaMixGpd pGammaMixGpd rGammaMixGpd qGammaMixGpd
#' @importFrom stats runif uniroot
NULL

#' @describeIn gamma_mixgpd Gamma mixture + GPD tail density
#' @export
dGammaMixGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 shape = double(1),
                 scale = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

          if (x < threshold) return(dGammaMix_nf(x, w, shape, scale, log))

          Fu <- pGammaMix_nf(threshold, w, shape, scale, 1, 0)
    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)

    if (log == 1) return(log(val))
    return(val)
  }
)

#' @describeIn gamma_mixgpd Gamma mixture + GPD tail distribution function
#' @export
pGammaMixGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 shape = double(1),
                 scale = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

          if (q < threshold) return(pGammaMix_nf(q, w, shape, scale, lower.tail, log.p))

          Fu <- pGammaMix_nf(threshold, w, shape, scale, 1, 0)
    G  <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)

    cdf <- Fu + (1.0 - Fu) * G

    if (is.nan(cdf)) cdf <- 0.0
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn gamma_mixgpd Gamma mixture + GPD tail random generation
#' @export
rGammaMixGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 shape = double(1),
                 scale = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pGammaMix_nf(threshold, w, shape, scale, 1, 0)
    u  <- runif(1, 0.0, 1.0)

    if (u < Fu) return(rGammaMix_nf(1, w, shape, scale))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)

#' @describeIn gamma_mixgpd Gamma mixture + GPD tail quantile function
#' @export
qGammaMixGpd <- function(p, w, shape, scale, threshold, tail_scale, tail_shape,
                         lower.tail = TRUE, log.p = FALSE,
                         tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- pGammaMix(threshold, w, shape, scale, 1, 0)
  out <- numeric(length(p))

  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- qGammaMix(pi, w, shape, scale, lower.tail = TRUE, log.p = FALSE, tol = tol, maxiter = maxiter)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}

#' Gamma with a GPD tail
#'
#' Spliced family obtained by attaching a generalized Pareto tail above `threshold` to a single
#' gamma bulk distribution.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param scale Numeric scalar scale parameter for the Gamma bulk.
#' @param shape Numeric scalar Gamma shape parameter.
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are
#'   \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#' @param tol Numeric tolerance for numerical inversion in \code{qGammaGpd}.
#' @param maxiter Maximum iterations for numerical inversion in \code{qGammaGpd}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars.
#'   `qGammaGpd()` returns a numeric vector with the same length as `p`.
#'
#' @seealso [gamma_mix()], [gamma_mixgpd()], [gpd()], [gamma_lowercase()].
#' @family gamma kernel families
#'
#' @examples
#' scale <- 2.5
#' shape <- 4
#' threshold <- 3
#' tail_scale <- 0.9
#' tail_shape <- 0.2
#'
#' dGammaGpd(4.0, scale = scale, shape = shape,
#'          threshold = threshold, tail_scale = tail_scale,
#'          tail_shape = tail_shape, log = 0)
#' pGammaGpd(4.0, scale = scale, shape = shape,
#'          threshold = threshold, tail_scale = tail_scale,
#'          tail_shape = tail_shape, lower.tail = 1, log.p = 0)
#' qGammaGpd(0.50, scale = scale, shape = shape,
#'          threshold = threshold, tail_scale = tail_scale,
#'          tail_shape = tail_shape)
#' qGammaGpd(0.95, scale = scale, shape = shape,
#'          threshold = threshold, tail_scale = tail_scale,
#'          tail_shape = tail_shape)
#' replicate(10, rGammaGpd(1, scale = scale, shape = shape,
#'                        threshold = threshold,
#'                        tail_scale = tail_scale,
#'                        tail_shape = tail_shape))
#'
#' @rdname gamma_gpd
#' @name gamma_gpd
#' @aliases dGammaGpd pGammaGpd rGammaGpd qGammaGpd
NULL


#' @describeIn gamma_gpd Gamma + GPD tail density
#' @export
dGammaGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 shape = double(0),
                 scale = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    if (x < threshold) return(dgamma(x, shape = shape, scale = scale, log = log))

    Fu <- pgamma(threshold, shape = shape, scale = scale, lower.tail = 1, log.p = 0)
    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)

    if (log == 1) return(log(val))
    return(val)
  }
)

#' @describeIn gamma_gpd Gamma + GPD tail distribution function
#' @export
pGammaGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 shape = double(0),
                 scale = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    if (q < threshold) return(pgamma(q, shape = shape, scale = scale, lower.tail = lower.tail, log.p = log.p))

    Fu <- pgamma(threshold, shape = shape, scale = scale, lower.tail = 1, log.p = 0)
    G  <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)

    cdf <- Fu + (1.0 - Fu) * G

    if (is.nan(cdf)) cdf <- 0.0
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn gamma_gpd Gamma + GPD tail random generation
#' @export
rGammaGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 shape = double(0),
                 scale = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pgamma(threshold, shape = shape, scale = scale, lower.tail = 1, log.p = 0)
    u  <- runif(1, 0.0, 1.0)

    if (u < Fu) return(rgamma(1, shape = shape, scale = scale))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)

#' @describeIn gamma_gpd Gamma + GPD tail quantile function
#' @export
#' @param tol Numeric tolerance for numerical inversion in \code{qGammaGpd}.
#' @param maxiter Maximum iterations for numerical inversion in \code{qGammaGpd}.
qGammaGpd <- function(p, shape, scale, threshold, tail_scale, tail_shape,
                      lower.tail = TRUE, log.p = FALSE,
                      tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- stats::pgamma(threshold, shape = shape, scale = scale, lower.tail = TRUE, log.p = FALSE)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- stats::qgamma(pi, shape = shape, scale = scale, lower.tail = TRUE, log.p = FALSE)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}

# ==========================================================
# Lowercase vectorized R wrappers for Gamma kernels
# ==========================================================

#' Lowercase vectorized gamma distribution functions
#'
#' Vectorized R wrappers for the scalar gamma-kernel topics in this file.
#'
#' @param x Numeric vector of quantiles.
#' @param q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Integer number of observations to generate.
#' @param w Numeric vector of mixture weights.
#' @param shape,scale Numeric vectors (mix) or scalars (base+gpd) of component parameters.
#' @param threshold,tail_scale,tail_shape GPD tail parameters (scalars).
#' @param log Logical; if \code{TRUE}, return log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le x)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are on log scale.
#' @param tol,maxiter Tolerance and max iterations for numerical inversion.
#'
#' @return Numeric vector of densities, probabilities, quantiles, or random variates.
#'
#' @seealso [gamma_mix()], [gamma_mixgpd()], [gamma_gpd()], [bundle()], [get_kernel_registry()].
#' @family vectorized kernel helpers
#'
#' @examples
#' w <- c(0.55, 0.3, 0.15)
#' shp <- c(2, 4, 6)
#' scl <- c(1, 2.5, 5)
#'
#' # Gamma mixture
#' dgammamix(c(1, 2, 3), w = w, shape = shp, scale = scl)
#' rgammamix(5, w = w, shape = shp, scale = scl)
#'
#' # Gamma mixture + GPD
#' dgammamixgpd(c(2, 3, 4), w = w, shape = shp, scale = scl,
#'              threshold = 3, tail_scale = 0.9, tail_shape = 0.2)
#'
#' # Gamma + GPD (single component)
#' dgammagpd(c(2, 3, 4), shape = 4, scale = 2.5, threshold = 3,
#'           tail_scale = 0.9, tail_shape = 0.2)
#'
#' @name gamma_lowercase
#' @rdname gamma_lowercase
NULL

# ---- Gamma Mix lowercase wrappers ----

#' @describeIn gamma_lowercase Gamma mixture density (vectorized)
#' @export
dgammamix <- function(x, w, shape, scale, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dGammaMix_nf(xi, w = w, shape = shape, scale = scale, log = log_int)),
         numeric(1L))
}

#' @describeIn gamma_lowercase Gamma mixture distribution function (vectorized)
#' @export
pgammamix <- function(q, w, shape, scale, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pGammaMix_nf(qi, w = w, shape = shape, scale = scale,
                                                  lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn gamma_lowercase Gamma mixture quantile function (vectorized)
#' @export
qgammamix <- function(p, w, shape, scale, lower.tail = TRUE, log.p = FALSE,
                      tol = 1e-10, maxiter = 200) {
  qGammaMix(p, w = w, shape = shape, scale = scale, lower.tail = lower.tail,
            log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn gamma_lowercase Gamma mixture random generation (vectorized)
#' @export
rgammamix <- function(n, w, shape, scale) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rGammaMix_nf(1L, w = w, shape = shape, scale = scale)),
         numeric(1L))
}

# ---- Gamma Mix + GPD lowercase wrappers ----

#' @describeIn gamma_lowercase Gamma mixture + GPD density (vectorized)
#' @export
dgammamixgpd <- function(x, w, shape, scale, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dGammaMixGpd(xi, w = w, shape = shape, scale = scale,
                                                  threshold = threshold, tail_scale = tail_scale,
                                                  tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn gamma_lowercase Gamma mixture + GPD distribution function (vectorized)
#' @export
pgammamixgpd <- function(q, w, shape, scale, threshold, tail_scale, tail_shape,
                         lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pGammaMixGpd(qi, w = w, shape = shape, scale = scale,
                                                  threshold = threshold, tail_scale = tail_scale,
                                                  tail_shape = tail_shape,
                                                  lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn gamma_lowercase Gamma mixture + GPD quantile function (vectorized)
#' @export
qgammamixgpd <- function(p, w, shape, scale, threshold, tail_scale, tail_shape,
                         lower.tail = TRUE, log.p = FALSE, tol = 1e-10, maxiter = 200) {
  qGammaMixGpd(p, w = w, shape = shape, scale = scale, threshold = threshold,
               tail_scale = tail_scale, tail_shape = tail_shape,
               lower.tail = lower.tail, log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn gamma_lowercase Gamma mixture + GPD random generation (vectorized)
#' @export
rgammamixgpd <- function(n, w, shape, scale, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rGammaMixGpd(1L, w = w, shape = shape, scale = scale,
                                                          threshold = threshold, tail_scale = tail_scale,
                                                          tail_shape = tail_shape)),
         numeric(1L))
}

# ---- Gamma + GPD lowercase wrappers ----

#' @describeIn gamma_lowercase Gamma + GPD density (vectorized)
#' @export
dgammagpd <- function(x, shape, scale, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dGammaGpd(xi, shape = shape, scale = scale,
                                               threshold = threshold, tail_scale = tail_scale,
                                               tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn gamma_lowercase Gamma + GPD distribution function (vectorized)
#' @export
pgammagpd <- function(q, shape, scale, threshold, tail_scale, tail_shape,
                      lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pGammaGpd(qi, shape = shape, scale = scale,
                                               threshold = threshold, tail_scale = tail_scale,
                                               tail_shape = tail_shape,
                                               lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn gamma_lowercase Gamma + GPD quantile function (vectorized)
#' @export
qgammagpd <- function(p, shape, scale, threshold, tail_scale, tail_shape,
                      lower.tail = TRUE, log.p = FALSE) {
  qGammaGpd(p, shape = shape, scale = scale, threshold = threshold,
            tail_scale = tail_scale, tail_shape = tail_shape,
            lower.tail = lower.tail, log.p = log.p)
}

#' @describeIn gamma_lowercase Gamma + GPD random generation (vectorized)
#' @export
rgammagpd <- function(n, shape, scale, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rGammaGpd(1L, shape = shape, scale = scale,
                                                       threshold = threshold, tail_scale = tail_scale,
                                                       tail_shape = tail_shape)),
         numeric(1L))
}

