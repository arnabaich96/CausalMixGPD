#' Gamma mixture distribution
#'
#' A finite mixture of Gamma components. Base Gamma functions are taken from \pkg{stats}.
#' Mixture density and CDF are computed by weighted sums. Random generation samples a component
#' according to weights and draws from the corresponding component. Quantiles are computed by
#' numerical inversion of the mixture CDF.
#' The \code{d*}, \code{p*}, and \code{q*} functions accept vector inputs for their first argument
#' and evaluate elementwise; \code{r*} supports \code{n > 1}.
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
#' @return Density/CDF/RNG functions return numeric scalars. \code{qGammaMix} returns a numeric vector
#'   with the same length as \code{p}.
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

    out[i] <- stats::uniroot(
      function(z) pGammaMix(z, w = w, shape = shape, scale = scale,
                            lower.tail = 1, log.p = 0) - pi,
      interval = c(0, 1e20),
      tol = tol, maxiter = maxiter
    )$root
  }
  out
}

#' Gamma mixture with a GPD tail
#'
#' Splices a generalized Pareto distribution (GPD) above \code{threshold} onto a Gamma mixture bulk.
#' The bulk probability at the threshold is used to scale the tail so that the overall CDF is proper.
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
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qGammaMixGpd} returns a numeric vector
#'   with the same length as \code{p}.
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

    if (x < threshold) return(dGammaMix(x, w, shape, scale, log))

    Fu <- pGammaMix(threshold, w, shape, scale, 1, 0)
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

    if (q < threshold) return(pGammaMix(q, w, shape, scale, lower.tail, log.p))

    Fu <- pGammaMix(threshold, w, shape, scale, 1, 0)
    G  <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)

    cdf <- Fu + (1.0 - Fu) * G

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

    Fu <- pGammaMix(threshold, w, shape, scale, 1, 0)
    u  <- runif(1, 0.0, 1.0)

    if (u < Fu) return(rGammaMix(1, w, shape, scale))
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
#' Splices a generalized Pareto distribution (GPD) above \code{threshold} onto a single
#' Gamma bulk distribution parameterized by scale \code{scale} and shape \code{shape}.
#' The bulk probability at the threshold, \eqn{F_{bulk}(threshold)}, scales the tail mass
#' so the overall CDF is proper.
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
#'   \code{qGammaGpd} returns a numeric vector with the same length as \code{p}.
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
