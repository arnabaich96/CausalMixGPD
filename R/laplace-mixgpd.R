#' Laplace mixture distribution
#'
#' This topic documents the finite-mixture version of the distribution.
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s so they can be used
#' inside NIMBLE models. The quantile function is an R function computed by numerical inversion
#' of the mixture CDF.
#'
#' Each component \eqn{j} is a Laplace (double-exponential) distribution with location \code{location[j]}
#' and scale \code{scale[j]}. The generic aliases \code{param1} and \code{param2} are accepted for these parameters.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE, RNG supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w} internally when needed.
#' @param location,scale Numeric vectors of length \eqn{K} giving component locations and scales.
#' @param param1,param2 Generic aliases for \code{location} and \code{scale}.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Density/CDF/RNG functions return numeric scalars. The quantile function returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' w <- c(0.5, 0.5)
#' loc <- c(0, 2)
#' sc <- c(1, 0.5)
#' dM <- compileNimble(dLaplaceMix)
#' dM(1, w = w, location = loc, scale = sc)
#' qLaplaceMix(0.9, w = w, location = loc, scale = sc)
#' }
#'
#' @rdname laplace_mix
#' @name laplace_mix
#' @aliases dLaplaceMix pLaplaceMix rLaplaceMix qLaplaceMix
NULL


#' Laplace mixture with a GPD tail
#'
#' This topic documents the mixture distribution spliced with a generalized Pareto (GPD) tail above
#' a threshold \code{u}. The bulk mixture governs \eqn{x<u}. For \eqn{x\ge u}, exceedances follow a GPD
#' and are weighted by \eqn{1 - F_{mix}(u)} so that the overall distribution remains proper.
#'
#' The bulk distribution is a Laplace mixture. A generalized Pareto tail is attached above \code{u}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE, RNG supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param location,scale Numeric vectors of length \eqn{K} giving component locations and scales.
#' @param param1,param2 Generic aliases for \code{location} and \code{scale}.
#' @param u Numeric scalar threshold at which the GPD tail is attached.
#' @param sigma Numeric scalar GPD scale parameter; must be positive.
#' @param xi Numeric scalar GPD shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in bulk quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. The quantile function returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' w <- c(0.5, 0.5)
#' loc <- c(0, 2)
#' sc <- c(1, 0.5)
#' u <- 2
#' dSp <- compileNimble(dLaplaceMixGPD)
#' dSp(3.0, w = w, location = loc, scale = sc, u = u, sigma = 1, xi = 0.2)
#' qLaplaceMixGPD(0.99, w = w, location = loc, scale = sc, u = u, sigma = 1, xi = 0.2)
#' }
#'
#' @rdname laplace_mixgpd
#' @name laplace_mixgpd
#' @aliases dLaplaceMixGPD pLaplaceMixGPD rLaplaceMixGPD qLaplaceMixGPD
NULL


#' @describeIn laplace_mix Density of Laplace mixture
#' @export
dLaplaceMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 param1 = double(1),
                 param2 = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(location) == 0) location <- param1
    if (length(scale) == 0) scale <- param2
    K <- length(w)
    s0 <- 0.0
    for (j in 1:K) s0 <- s0 + w[j] * ddexp(x, location[j], scale[j])
    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0)) else return(s0)
  }
)

#' @describeIn laplace_mix Cumulative distribution function of Laplace mixture
#' @export
pLaplaceMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 param1 = double(1),
                 param2 = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(location) == 0) location <- param1
    if (length(scale) == 0) scale <- param2
    K <- length(w)
    cdf <- 0.0
    for (j in 1:K) cdf <- cdf + w[j] * pdexp(q, location[j], scale[j])
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn laplace_mix Random generation from Laplace mixture
#' @export
rLaplaceMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 param1 = double(1),
                 param2 = double(1)) {
    returnType(double(0))
    if (length(location) == 0) location <- param1
    if (length(scale) == 0) scale <- param2
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
    return(rdexp(1, location[idx], scale[idx]))
  }
)

#' @describeIn laplace_mix Quantile function of Laplace mixture
#' @export
qLaplaceMix <- function(p, w, location, scale, param1 = location, param2 = scale,
                        lower.tail = TRUE, log.p = FALSE,
                        tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(location) == 0) location <- as.numeric(param1)
  if (length(scale) == 0) scale <- as.numeric(param2)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- -Inf; next }
    if (pi >= 1) { out[i] <- Inf; next }
    out[i] <- stats::uniroot(function(z) pLaplaceMix(z,w, location, scale) - pi, interval = c(-1e10,1e10), tol = tol, maxiter = maxiter)$root
  }
  out
}

#' @describeIn laplace_mixgpd Density of Laplace mixture with GPD tail
#' @export
dLaplaceMixGPD <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (x < u) return(dLaplaceMix(x, w, location, scale, param1, param2, log))
    Fu <- pLaplaceMix(u, w, location, scale, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    val <- (1.0 - Fu) * dGpd(x, u, sigma, xi, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn laplace_mixgpd Cumulative distribution function of Laplace mixture with GPD tail
#' @export
pLaplaceMixGPD <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (q < u) return(pLaplaceMix(q, w, location, scale, param1, param2, lower.tail, log.p))
    Fu <- pLaplaceMix(u, w, location, scale, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    G <- pGpd(q, u, sigma, xi, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn laplace_mixgpd Random generation from Laplace mixture with GPD tail
#' @export
rLaplaceMixGPD <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0)) {
    returnType(double(0))
    if (n != 1) return(0.0)
    Fu <- pLaplaceMix(u, w, location, scale, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rLaplaceMix(1, w, location, scale, param1, param2))
    return(rGpd(1, u, sigma, xi))
  }
)

#' @describeIn laplace_mixgpd Quantile function of Laplace mixture with GPD tail
#' @export
qLaplaceMixGPD <- function(p, w, location, scale, u, sigma, xi,
                           param1 = location, param2 = scale,
                           lower.tail = TRUE, log.p = FALSE,
                           tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(location) == 0) location <- as.numeric(param1)
  if (length(scale) == 0) scale <- as.numeric(param2)
  Fu <- compileNimble(pLaplaceMix)(u, w=w, location=location, scale=scale)
  Fu <- max(min(Fu, 1.0), 0.0)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) out[i] <- qLaplaceMix(pi, w, location, scale, lower.tail=TRUE, log.p=FALSE, tol=tol, maxiter=maxiter)
    else {
      g <- if (Fu >= 1) 0 else (pi - Fu)/(1 - Fu)
      out[i] <- qGpd(g, u = u, sigma = sigma, xi = xi)
    }
  }
  out
}
