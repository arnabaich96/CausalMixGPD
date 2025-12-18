#' Gamma mixture distribution
#'
#' This topic documents the finite-mixture version of the distribution.
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s so they can be used
#' inside NIMBLE models. The quantile function is an R function computed by numerical inversion
#' of the mixture CDF.
#'
#' Each component \eqn{j} is a Gamma distribution with \code{shape[j]} and \code{scale[j]}.
#' The argument \code{rate} can be provided instead of \code{scale} (interpreted as \code{scale = 1/rate}).
#' The generic aliases \code{param1} and \code{param2} are accepted for \code{shape} and \code{scale}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE, RNG supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w} internally when needed.
#' @param shape,scale Numeric vectors of length \eqn{K} giving component shapes and scales.
#' @param rate Numeric vector of length \eqn{K} giving component rates (optional alternative to \code{scale}).
#' @param param1,param2 Generic aliases for \code{shape} and \code{scale}.
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
#' w <- c(0.8, 0.2)
#' shape <- c(2, 8)
#' scale <- c(1, 0.5)
#' dM <- compileNimble(dGammaMix)
#' dM(1.2, w = w, shape = shape, scale = scale)
#' qGammaMix(0.9, w = w, shape = shape, scale = scale)
#' }
#'
#' @rdname gamma_mix
#' @name gamma_mix
#' @aliases dGammaMix pGammaMix rGammaMix qGammaMix
NULL


#' Gamma mixture with a GPD tail
#'
#' This topic documents the mixture distribution spliced with a generalized Pareto (GPD) tail above
#' a threshold \code{u}. The bulk mixture governs \eqn{x<u}. For \eqn{x\ge u}, exceedances follow a GPD
#' and are weighted by \eqn{1 - F_{mix}(u)} so that the overall distribution remains proper.
#'
#' The bulk distribution is a Gamma mixture. A generalized Pareto tail is attached above \code{u}.
#' The tail mass is \eqn{1 - F_{mix}(u)}, where \eqn{F_{mix}} is the Gamma mixture CDF.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE, RNG supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param shape,scale Numeric vectors of length \eqn{K} giving component shapes and scales.
#' @param rate Numeric vector of length \eqn{K} giving component rates (optional alternative to \code{scale}).
#' @param param1,param2 Generic aliases for \code{shape} and \code{scale}.
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
#' w <- c(0.8, 0.2)
#' shape <- c(2, 8)
#' scale <- c(1, 0.5)
#' u <- 2
#' dSp <- compileNimble(dGammaMixGPD)
#' dSp(3.0, w = w, shape = shape, scale = scale, u = u, sigma = 1, xi = 0.2)
#' qGammaMixGPD(0.99, w = w, shape = shape, scale = scale, u = u, sigma = 1, xi = 0.2)
#' }
#'
#' @rdname gamma_mixgpd
#' @name gamma_mixgpd
#' @aliases dGammaMixGPD pGammaMixGPD rGammaMixGPD qGammaMixGPD
NULL



#' @describeIn gamma_mix Density of Gamma mixture.
#' @export
dGammaMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 shape = double(1),
                 scale = double(1),
                 rate = double(1,default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(shape) == 0) shape <- param1
    if (length(scale) == 0) scale <- param2
    if (length(scale) == 0 & length(rate) > 0) scale <- 1.0 / rate
    K <- length(w)
    s <- 0.0
    for (j in 1:K) {
      s <- s + w[j] * dgamma(x, shape = shape[j], scale = scale[j], log = 0)
    }
    if (s < eps) s <- eps
    if (log == 1) return(log(s)) else return(s)
  }
)

#' @describeIn gamma_mix Distribution Function of Gamma mixture.
#' @export
pGammaMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 shape = double(1),
                 scale = double(1),
                 rate = double(1,default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(shape) == 0) shape <- param1
    if (length(scale) == 0) scale <- param2
    if (length(scale) == 0 & length(rate) > 0) scale <- 1.0 / rate
    K <- length(w)
    cdf <- 0.0
    for (j in 1:K) {
      cdf <- cdf + w[j] * pgamma(q, shape = shape[j], scale = scale[j], lower.tail = 1, log.p = 0)
    }
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn gamma_mix Random Generation of Gamma mixture.
#' @export
rGammaMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 shape = double(1),
                 scale = double(1),
                 rate  = double(1,default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1)

  ) {
    returnType(double(0))

    if (n != 1) return(0.0)
    K <- length(w)
    sumw <- 0.0
    for (k in 1:K) sumw <- sumw + w[k]
    if (sumw <= 0.0) return(0.0)

    u <- runif(1, 0.0, sumw)
    cw <- 0.0
    idx <- 1
    found <- 0L
    for (k in 1:K) {
      cw <- cw + w[k]
      if (found == 0L) {
        if (u <= cw) { idx <- k; found <- 1L }
      }
    }
    return(rgamma(1, shape = shape[idx], scale = scale[idx]))
  }
)




#' @describeIn gamma_mix Quantile Function of Gamma mixture.
#' @export
qGammaMix <- function(p, w, shape, scale, rate = NULL, param1 = shape, param2 = scale,
                      lower.tail = TRUE, log.p = FALSE,
                      tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(shape) == 0) shape <- as.numeric(param1)
  if (length(scale) == 0) scale <- as.numeric(param2)
  if ((length(scale) == 0) & !is.null(rate)) scale <- 1 / as.numeric(rate)
  if (length(w) != length(shape) || length(w) != length(scale)) stop("Lengths must match.", call. = FALSE)

  cdf_mix <- function(z) sum(w * stats::pgamma(z, shape = shape, scale = scale))
  # bracket using component quantiles
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- 0; next }
    if (pi >= 1) { out[i] <- Inf; next }
    out[i] <- stats::uniroot(function(z) cdf_mix(z) - pi, interval = c(-1,1e10), tol = tol, maxiter = maxiter)$root
  }
  out
}

#' @describeIn gamma_mixgpd Density of Gamma mixture with GPD tail
#' @export
dGammaMixGPD <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 shape = double(1),
                 scale = double(1),
                 rate = double(1,default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (x < u) return(dGammaMix(x, w, shape, scale, rate, param1, param2, log))
    Fu <- pGammaMix(u, w, shape, scale, rate, param1, param2, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0
    val <- (1.0 - Fu) * dGpd(x, u, sigma, xi, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn gamma_mixgpd Distribution Function of Gamma mixture with GPD Tail.
#' @export
pGammaMixGPD <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 shape = double(1),
                 scale = double(1),
                 rate = double(1,default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (q < u) return(pGammaMix(q, w, shape, scale, rate, param1, param2, lower.tail, log.p))
    Fu <- pGammaMix(u, w, shape, scale, rate, param1, param2, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0
    G <- pGpd(q, u, sigma, xi, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn gamma_mixgpd random generation(n=1) of Gamma mixture with GPD tail.
#' @export
rGammaMixGPD <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 shape = double(1),
                 scale = double(1),
                 rate = double(1,default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0)) {
    returnType(double(0))
    if (n != 1) return(0.0)
    Fu <- pGammaMix(u, w, shape, scale, rate, param1, param2, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0
    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rGammaMix(1, w, shape, scale, rate, param1, param2))
    return(rGpd(1, u, sigma, xi))
  }
)

#' @describeIn gamma_mixgpd Quantile Function of Gamma mixture with GPD tail.
#' @export
qGammaMixGPD <- function(p, w, shape, scale, u, sigma, xi,
                         rate = NULL, param1 = shape, param2 = scale,
                         lower.tail = TRUE, log.p = FALSE,
                         tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(shape) == 0) shape <- as.numeric(param1)
  if (length(scale) == 0) scale <- as.numeric(param2)
  if ((length(scale) == 0) & !is.null(rate)) scale <- 1 / as.numeric(rate)
  Fu <- sum(w * stats::pgamma(u, shape = shape, scale = scale))
  Fu <- max(min(Fu, 1), 0)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) out[i] <- qGammaMix(pi, w, shape, scale, lower.tail = TRUE, log.p = FALSE, tol = tol, maxiter = maxiter)
    else {
      g <- if (Fu >= 1) 0 else (pi - Fu)/(1 - Fu)
      out[i] <- qGpd(g, u = u, sigma = sigma, xi = xi)
    }
  }
  out
}

