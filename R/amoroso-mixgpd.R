#' Amoroso distribution
#'
#' The Amoroso family is a flexible four-parameter distribution on a half-line that
#' includes many common right-skewed models as special cases. In this package it is
#' used as a positive-support kernel that plays nicely with the peaks-over-threshold
#' splicing used by the MixGPD constructions.
#'
#' This topic documents the core Amoroso functions. The density, distribution, and
#' random-generation functions are implemented as \code{nimbleFunction}s so they can
#' be called from NIMBLE models and compiled. The quantile function is an ordinary
#' R function and can be used directly in analysis code.
#'
#' The parameterization follows \code{loc} (location), \code{scale} (scale), and two
#' shape parameters \code{shape1} and \code{shape2}. The scale is allowed to be negative,
#' which flips the support around \code{loc}. When \code{scale > 0}, the support is
#' \eqn{x \ge loc}; when \code{scale < 0}, the support is \eqn{x \le loc}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param loc Numeric scalar location parameter.
#' @param scale Numeric scalar scale parameter. A negative value flips the support.
#' @param shape1 Numeric scalar first shape parameter. Values must be positive for a proper density.
#' @param shape2 Numeric scalar second shape parameter. The sign affects tail behavior and can flip
#'   the monotonicity of the gamma transform used internally.
#' @param log Logical; if \code{TRUE}, return the log-density. In NIMBLE functions this is provided
#'   as an integer flag with \code{0/1}.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return \code{dAmoroso} returns a numeric scalar density; \code{pAmoroso} returns a numeric scalar CDF;
#'   \code{qAmoroso} returns a numeric scalar quantile; \code{rAmoroso} returns one random draw.
#'
#' @seealso \code{dAmorosoMix}, \code{dAmorosoMixGPD} for mixture and spliced variants.
#'
#' @rdname amoroso
#' @name amoroso
#' @aliases dAmoroso pAmoroso qAmoroso rAmoroso
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
    z <- (x - loc) / scale
    lik <- abs(shape2/scale) * (z^(shape1*shape2 - 1.0)) * exp(-z^shape2) / gamma(shape1)
    if (scale < 0) {
      if (x > loc) lik <- 0.0
    } else {
      if (x < loc) lik <- 0.0
    }
    if (lik < eps) lik <- eps
    if (log == 1) return(log(lik)) else return(lik)
  }
)
#' @rdname amoroso
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
    z <- ((q - loc) / scale)^shape2
    cdf <- pgamma(z, shape = shape1, scale = 1.0)
    if (scale < 0) {
      if (q > loc) cdf <- 0.0
    } else {
      if (q < loc) cdf <- 0.0
    }
    if (shape2 < 0) cdf <- 1.0 - cdf
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)
#' @rdname amoroso
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
#' @rdname amoroso
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
    return(loc + scale * pow(z, 1.0/shape2))
  }
)

#' Amoroso mixture distribution
#'
#' A finite mixture of Amoroso components is often a convenient bulk model when the response is
#' positive and right-skewed. These functions evaluate and simulate from
#' \eqn{\sum_j w_j f_{Amoroso}(\cdot\mid loc_j, scale_j, shape1_j, shape2_j)}.
#'
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s so they can be called from
#' NIMBLE models. The quantile function is provided as an R function and is computed by numerical
#' inversion of the mixture CDF.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions treat the weights
#'   as non-negative and normalize them internally when needed.
#' @param loc Numeric vector of length \eqn{K} giving component locations.
#' @param scale Numeric vector of length \eqn{K} giving component scales. Negative values flip support
#'   for the corresponding component.
#' @param shape1 Numeric vector of length \eqn{K} giving the first Amoroso shape parameter for each component.
#' @param shape2 Numeric vector of length \eqn{K} giving the second Amoroso shape parameter for each component.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Mixture density/CDF/RNG functions return numeric scalars. \code{qAmorosoMix} returns a numeric
#'   vector with the same length as \code{p}.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' w <- c(0.7, 0.3)
#' loc <- c(0, 1)
#' scale <- c(1, 1.5)
#' shape1 <- c(2, 5)
#' shape2 <- c(1, 2)
#'
#' dMix <- compileNimble(dAmorosoMix)
#' dMix(0.5, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2)
#'
#' qAmorosoMix(0.9, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2)
#' }
#'
#' @rdname amoroso_mix
#' @name amoroso_mix
#' @aliases dAmorosoMix pAmorosoMix rAmorosoMix qAmorosoMix
NULL


#' Amoroso mixture with a GPD tail
#'
#' This family splices a generalized Pareto distribution (GPD) above a threshold \code{u} onto an
#' Amoroso mixture bulk. Let \eqn{F_{mix}} denote the Amoroso mixture CDF. The spliced CDF is
#' \eqn{F(x)=F_{mix}(x)} for \eqn{x<u} and
#' \eqn{F(x)=F_{mix}(u) + \left\{1-F_{mix}(u)\right\}G(x)} for \eqn{x\ge u}, where \eqn{G}
#' is the GPD CDF for exceedances above \code{u}. This preserves total probability and makes it
#' clear how the tail mass depends on \eqn{1-F_{mix}(u)}.
#'
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s for use in NIMBLE models.
#' The quantile function is an R function that uses numerical inversion in the bulk region and
#' the closed-form GPD quantile in the tail region.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions treat the weights
#'   as non-negative and normalize them internally when needed.
#' @param loc Numeric vector of length \eqn{K} giving component locations.
#' @param scale Numeric vector of length \eqn{K} giving component scales.
#' @param shape1 Numeric vector of length \eqn{K} giving the first Amoroso shape parameter for each component.
#' @param shape2 Numeric vector of length \eqn{K} giving the second Amoroso shape parameter for each component.
#' @param u Numeric scalar threshold at which the GPD tail is attached.
#' @param sigma Numeric scalar GPD scale parameter; must be positive.
#' @param xi Numeric scalar GPD shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qAmorosoMixGPD} returns a numeric
#'   vector with the same length as \code{p}.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' w <- c(0.7, 0.3)
#' loc <- c(0, 1)
#' scale <- c(1, 1.5)
#' shape1 <- c(2, 5)
#' shape2 <- c(1, 2)
#' u <- 2
#'
#' dSp <- compileNimble(dAmorosoMixGPD)
#' dSp(3.0, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
#'     u = u, sigma = 1, xi = 0.2)
#'
#' qAmorosoMixGPD(0.99, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
#'               u = u, sigma = 1, xi = 0.2)
#' }
#'
#' @rdname amoroso_mixgpd
#' @name amoroso_mixgpd
#' @aliases dAmorosoMixGPD pAmorosoMixGPD rAmorosoMixGPD qAmorosoMixGPD
NULL



#' Amoroso mixture density (NIMBLE)
#' @describeIn amoroso_mix Density Function of Amoroso Mixture Distribution
#' @export
dAmorosoMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 loc = double(1),
                 scale = double(1),
                 shape1 = double(1),
                 shape2 = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    K <- length(w)
    s0 <- 0.0
    for (j in 1:K) s0 <- s0 + w[j] * dAmoroso(x, loc[j], scale[j], shape1[j], shape2[j], 0)
    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0)) else return(s0)
  }
)

#' Amoroso mixture CDF (NIMBLE)
#' @describeIn amoroso_mix Cumulative Distribution Function of Amoroso Mixture Distribution
#' @export
pAmorosoMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 loc = double(1),
                 scale = double(1),
                 shape1 = double(1),
                 shape2 = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    K <- length(w)
    cdf <- 0.0
    for (j in 1:K) cdf <- cdf + w[j] * pAmoroso(q, loc[j], scale[j], shape1[j], shape2[j], 1, 0)
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' Amoroso mixture RNG (NIMBLE)
#' @describeIn amoroso_mix Random Generation for Amoroso Mixture Distribution
#' @export
rAmorosoMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 loc = double(1),
                 scale = double(1),
                 shape1 = double(1),
                 shape2 = double(1)) {
    returnType(double(0))
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
    return(rAmoroso(1, loc[idx], scale[idx], shape1[idx], shape2[idx]))
  }
)

#' Amoroso mixture quantile (R)
#' @describeIn amoroso_mix Quantile Function of Amoroso Mixture Distribution
#' @export
qAmorosoMix <- function(p, w, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE,
                        tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  a0 <- min(loc) - 10*abs(scale)
  b0 <- max(loc) + 10*abs(scale)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- a0; next }
    if (pi >= 1) { out[i] <- b0; next }
    out[i] <- uniroot(function(z) pAmorosoMix(z,w, loc, scale, shape1, shape2) - pi, interval = c(-1e10,1e10), tol = tol, maxiter = maxiter)$root
  }
  out
}

#' Amoroso mixture + GPD density (NIMBLE)
#' @describeIn amoroso_mixgpd Density Function of Amoroso Mixture Distribution with GPD Tail
#' @export
dAmorosoMixGPD <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 loc = double(1),
                 scale = double(1),
                 shape1 = double(1),
                 shape2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (x < u) return(dAmorosoMix(x, w, loc, scale, shape1, shape2, log))
    Fu <- pAmorosoMix(u, w, loc, scale, shape1, shape2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    val <- (1.0 - Fu) * dGpd(x, u, sigma, xi, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' Amoroso mixture + GPD CDF (NIMBLE)
#' @describeIn amoroso_mixgpd Cumulative Distribution Function of Amoroso Mixture Distribution with GPD Tail
#' @export
pAmorosoMixGPD <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 loc = double(1),
                 scale = double(1),
                 shape1 = double(1),
                 shape2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (q < u) return(pAmorosoMix(q, w, loc, scale, shape1, shape2, lower.tail, log.p))
    Fu <- pAmorosoMix(u, w, loc, scale, shape1, shape2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    G <- pGpd(q, u, sigma, xi, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' Amoroso mixture + GPD RNG (NIMBLE)
#' @describeIn amoroso_mixgpd Random Generation for Amoroso Mixture Distribution with GPD Tail
#' @export
rAmorosoMixGPD <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 loc = double(1),
                 scale = double(1),
                 shape1 = double(1),
                 shape2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0)) {
    returnType(double(0))
    if (n != 1) return(0.0)
    Fu <- pAmorosoMix(u, w, loc, scale, shape1, shape2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rAmorosoMix(1, w, loc, scale, shape1, shape2))
    return(rGpd(1, u, sigma, xi))
  }
)

#' Amoroso mixture + GPD quantile (R)
#' @describeIn amoroso_mixgpd Quantile Function of Amoroso Mixture Distribution with GPD Tail
#' @export
qAmorosoMixGPD <- function(p, w, loc, scale, shape1, shape2, u, sigma, xi,
                           lower.tail = TRUE, log.p = FALSE,
                           tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  # compute Fu via R-level mixture CDF at u
  Fu <- sum(w * sapply(seq_along(w), function(j) {
    zt <- ((u - loc[j]) / scale[j])^shape2[j]
    cdf <- pgamma(zt, shape = shape1[j], scale = 1)
    if (scale[j] < 0) { if (u > loc[j]) cdf <- 0 } else { if (u < loc[j]) cdf <- 0 }
    if (shape2[j] < 0) cdf <- 1 - cdf
    cdf
  }))
  Fu <- max(min(Fu, 1), 0)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) out[i] <- qAmorosoMix(pi, w, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE, tol = tol, maxiter = maxiter)
    else {
      g <- if (Fu >= 1) 0 else (pi - Fu)/(1 - Fu)
      out[i] <- qGpd(g, u = u, sigma = sigma, xi = xi)
    }
  }
  out
}

