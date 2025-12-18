#' Pareto distribution
#'
#' The Pareto distribution is a classic heavy-tailed model on \eqn{[scale, \infty)}.
#' In this package it is provided as a custom kernel for NIMBLE, with a simple
#' \code{scale}/\code{shape} parameterization. The implementation is useful both on its own
#' and as a building block for finite mixtures.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function (if provided).
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param scale Numeric scalar scale (lower bound) parameter; must be positive.
#' @param shape Numeric scalar shape parameter; must be positive.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return \code{dPareto} returns a numeric scalar density; \code{pPareto} returns a numeric scalar CDF;
#'   \code{rPareto} returns one random draw.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' dP <- compileNimble(dPareto)
#' dP(3, scale = 1, shape = 2)
#' }
#'
#' @rdname pareto
#' @name pareto
#' @aliases dPareto pPareto rPareto
dPareto <- nimble::nimbleFunction(
  run = function(x = double(0),
                 scale = double(0),
                 shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    if (scale <= 0.0) {
      if (log == 1) return(-1.0e300) else return(0.0)
    }
    if (shape <= 0.0) {
      if (log == 1) return(-1.0e300) else return(0.0)
    }

    if (x < scale) {
      if (log == 1) return(-1.0e300) else return(0.0)
    }

    logdens <- log(shape) + shape * log(scale) - (shape + 1.0) * log(x)
    if (log == 1) return(logdens)
    return(exp(logdens))
  }
)
#' @rdname pareto
#' @export

pPareto <- nimble::nimbleFunction(
  run = function(q = double(0),
                 scale = double(0),
                 shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    eps <- 1e-300

    if (scale <= 0.0) {
      if (log.p != 0) return(log(eps)) else return(eps)
    }
    if (shape <= 0.0) {
      if (log.p != 0) return(log(eps)) else return(eps)
    }

    cdf <- 0.0
    if (q < scale) {
      cdf <- 0.0
    } else {
      cdf <- 1.0 - exp(shape * (log(scale) - log(q)))
      if (cdf < 0.0) cdf <- 0.0
      if (cdf > 1.0) cdf <- 1.0
    }

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) return(log(max(cdf, eps)))
    return(cdf)
  }
)
#' @rdname pareto
#' @export

rPareto <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 scale = double(0),
                 shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)
    if (scale <= 0.0 & shape <= 0.0) return(0.0)

    u <- runif(1, 0.0, 1.0)

    # inverse CDF: x = scale / (1-u)^(1/shape)
    return(scale / pow(1.0 - u, 1.0 / shape))
  }
)

#' Pareto mixture distribution
#'
#' A Pareto mixture is a finite mixture of Pareto(scale, shape) components. This can be used as a
#' simple heavy-tailed model when multiple tail regimes are plausible. This file intentionally does
#' not provide a GPD-spliced version, because a Pareto component already has a power-law tail and
#' splicing would typically be redundant.
#'
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s for use inside NIMBLE models.
#' The quantile function is an R function computed by numerical inversion of the mixture CDF.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w}
#'   internally when needed.
#' @param scale,shape Numeric vectors of length \eqn{K} giving component scales and shapes.
#' @param xm,alpha Alternative names for \code{scale} and \code{shape}, respectively.
#' @param param1,param2 Generic aliases for \code{scale} and \code{shape}.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Density/CDF/RNG functions return numeric scalars. \code{qParetoMix} returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' w <- c(0.5, 0.5)
#' sc <- c(1, 2)
#' sh <- c(2, 4)
#' dM <- compileNimble(dParetoMix)
#' dM(3, w = w, scale = sc, shape = sh)
#' qParetoMix(0.9, w = w, scale = sc, shape = sh)
#' }
#'
#' @rdname pareto_mix
#' @name pareto_mix
#' @aliases dParetoMix pParetoMix rParetoMix qParetoMix
NULL


#' @describeIn pareto_mix Density Function for Pareto Mixture
#' @export
dParetoMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 scale = double(1),
                 shape = double(1),
                 xm = double(1, default = numeric(0)),
                 alpha = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(scale) == 0) scale <- param1
    if (length(shape) == 0) shape <- param2
    if (length(scale) == 0 & length(xm) > 0) scale <- xm
    if (length(shape) == 0 & length(alpha) > 0) shape <- alpha
    K <- length(w)
    s0 <- 0.0
    for (j in 1:K) s0 <- s0 + w[j] * dPareto(x, scale[j], shape[j], 0)
    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0)) else return(s0)
  }
)

#' @describeIn pareto_mix Distribution Function for Pareto Mixture
#' @export
pParetoMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 scale = double(1),
                 shape = double(1),
                 xm = double(1, default = numeric(0)),
                 alpha = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(scale) == 0) scale <- param1
    if (length(shape) == 0) shape <- param2
    if (length(scale) == 0 & length(xm) > 0) scale <- xm
    if (length(shape) == 0 & length(alpha) > 0) shape <- alpha
    K <- length(w)
    cdf <- 0.0
    for (j in 1:K) cdf <- cdf + w[j] * pPareto(q, scale[j], shape[j], 1, 0)
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn pareto_mix Random Generation for Pareto Mixture
#' @export
rParetoMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 scale = double(1),
                 shape = double(1),
                 xm = double(1, default = numeric(0)),
                 alpha = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1)) {
    returnType(double(0))
    if (length(scale) == 0) scale <- param1
    if (length(shape) == 0) shape <- param2
    if (length(scale) == 0 & length(xm) > 0) scale <- xm
    if (length(shape) == 0 & length(alpha) > 0) shape <- alpha
    if (n != 1) return(0.0)
    K <- length(w)
    u <- runif(1, 0.0, 1.0)
    cw <- 0.0
    idx <- 1
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
    return(rPareto(1, scale[idx], shape[idx]))
  }
)

#' @describeIn pareto_mix Quantile Function for Pareto Mixture
#' @export
qParetoMix <- function(p, w, scale, shape, xm = NULL, alpha = NULL, param1 = scale, param2 = shape,
                       lower.tail = TRUE, log.p = FALSE,
                       tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(scale) == 0) scale <- as.numeric(param1)
  if (length(shape) == 0) shape <- as.numeric(param2)
  if (length(scale) == 0 & !is.null(xm)) scale <- as.numeric(xm)
  if (length(shape) == 0 & !is.null(alpha)) shape <- as.numeric(alpha)

  cdf_mix <- function(z) sum(w * (z < scale) * 0 + w * (z >= scale) * (1 - (scale/z)^shape))
  a0 <- min(scale)
  b0 <- max(scale / (1e-12)^(1/shape))
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- a0; next }
    if (pi >= 1) { out[i] <- Inf; next }

    out[i] <- stats::uniroot(function(z) pParetoMix(z,w, scale, shape) - pi, interval = c(-1,1e10), tol = tol, maxiter = maxiter)$root
  }
  out
}

