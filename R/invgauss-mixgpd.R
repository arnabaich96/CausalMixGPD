#' Inverse Gaussian (Wald) distribution
#'
#' The inverse Gaussian (also called the Wald distribution) is a positive-support model that is
#' right-skewed and often used for waiting times. This package provides NIMBLE-compatible density,
#' CDF, and RNG functions under the \code{mean}/\code{shape} parameterization
#' (mean \eqn{\mu>0}, shape \eqn{\lambda>0}). A standalone quantile function is not provided in this file;
#' mixture quantiles are computed by numerical inversion instead.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param mean Numeric scalar mean parameter \eqn{\mu>0}.
#' @param shape Numeric scalar shape parameter \eqn{\lambda>0}.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return \code{dinvgauss} returns a numeric scalar density; \code{pinvgauss} returns a numeric scalar CDF;
#'   \code{rinvgauss} returns one random draw.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' dIG <- compileNimble(dinvgauss)
#' dIG(1.2, mean = 1, shape = 2)
#' }
#'
#' @rdname invgauss
#' @name invgauss
#' @aliases dinvgauss pinvgauss rinvgauss
NULL

#' @describeIn invgauss Inverse Gaussian dennsity function
#' @export
dinvgauss <- nimble::nimbleFunction(
  run = function(x = double(0),
                 mean = double(0),
                 shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    if (mean <= 0.0) {
      if (log == 1) return(-1.0e300) else return(0.0)
    }
    if (shape <= 0.0) {
      if (log == 1) return(-1.0e300) else return(0.0)
    }
    if (x <= 0.0) {
      if (log == 1) return(-1.0e300) else return(0.0)
    }

    # log f(x) = 0.5*(log(shape) - log(2*pi) - 3*log(x))
    #            - shape*(x-mean)^2 / (2*mean^2*x)
    logdens <- 0.5 * (log(shape) - log(2.0 * pi) - 3.0 * log(x)) -
      shape * (x - mean) * (x - mean) / (2.0 * mean * mean * x)

    if (log == 1) return(logdens)
    return(exp(logdens))
  }
)
#' @rdname invgauss
#' @export

pinvgauss <- nimble::nimbleFunction(
  run = function(q = double(0),
                 mean = double(0),
                 shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    eps <- 1e-300

    if (mean <= 0.0) {
      if (log.p != 0) return(log(eps)) else return(eps)
    }
    if (shape <= 0.0) {
      if (log.p != 0) return(log(eps)) else return(eps)
    }

    cdf <- 0.0
    if (q <= 0.0) {
      cdf <- 0.0
    } else {
      # Standard IG CDF:
      # Phi( sqrt(lam/q) * (q/mu - 1) ) + exp(2*lam/mu) * Phi( -sqrt(lam/q) * (q/mu + 1) )
      t <- sqrt(shape / q)
      z1 <- t * (q / mean - 1.0)
      z2 <- -t * (q / mean + 1.0)
      cdf <- pnorm(z1, 0.0, 1.0, 1, 0) + exp(2.0 * shape / mean) * pnorm(z2, 0.0, 1.0, 1, 0)

      if (cdf < 0.0) cdf <- 0.0
      if (cdf > 1.0) cdf <- 1.0
    }

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) return(log(max(cdf, eps)))
    return(cdf)
  }
)
#' @rdname invgauss
#' @export

rinvgauss <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 mean = double(0),
                 shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)
    if (mean <= 0.0) return(0.0)
    if (shape <= 0.0) return(0.0)

    # Michael–Schucany–Haas algorithm
    v <- rnorm(1, 0.0, 1.0)
    y <- v * v
    mu <- mean
    lam <- shape

    term <- mu + (mu * mu * y) / (2.0 * lam) -
      (mu / (2.0 * lam)) * sqrt(4.0 * mu * lam * y + mu * mu * y * y)

    u <- runif(1, 0.0, 1.0)
    if (u <= mu / (mu + term)) return(term)
    return(mu * mu / term)
  }
)

#' Inverse Gaussian mixture distribution
#'
#' A finite mixture of inverse Gaussian components provides a flexible bulk model for positive data
#' with right skewness. Each component is parameterized by its mean \code{mean[j]} and shape
#' \code{shape[j]} (also accepted as \code{mu[j]} and \code{lambda[j]}).
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
#' @param mean,shape Numeric vectors of length \eqn{K} giving component means and shapes.
#' @param mu,lambda Alternative names for \code{mean} and \code{shape}.
#' @param param1,param2 Generic aliases for \code{mean} and \code{shape}.
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
#' \dontrun{
#' library(nimble)
#' w <- c(0.6, 0.4)
#' mu <- c(1, 3)
#' lam <- c(2, 5)
#' dM <- compileNimble(dInvGaussMix)
#' dM(1.2, w = w, mean = mu, shape = lam)
#' qInvGaussMix(0.9, w = w, mean = mu, shape = lam)
#' }
#'
#' @rdname invgauss_mix
#' @name invgauss_mix
#' @aliases dInvGaussMix pInvGaussMix rInvGaussMix qInvGaussMix
NULL


#' Inverse Gaussian mixture with a GPD tail
#'
#' This family splices a generalized Pareto distribution (GPD) above a threshold \code{u} onto an
#' inverse Gaussian mixture bulk. The bulk probability at the threshold, \eqn{F_{mix}(u)}, is used
#' to scale the tail so that the overall CDF remains proper.
#'
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s. The quantile function is an R
#' function that inverts the bulk mixture numerically and uses the closed-form GPD quantile in the tail.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param mean,shape Numeric vectors of length \eqn{K} giving component means and shapes.
#' @param mu,lambda Alternative names for \code{mean} and \code{shape}.
#' @param param1,param2 Generic aliases for \code{mean} and \code{shape}.
#' @param u Numeric scalar threshold at which the GPD tail is attached.
#' @param sigma Numeric scalar GPD scale parameter; must be positive.
#' @param xi Numeric scalar GPD shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qInvGaussMixGPD} returns a numeric
#'   vector with the same length as \code{p}.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' w <- c(0.6, 0.4)
#' mu <- c(1, 3)
#' lam <- c(2, 5)
#' u <- 2
#' dSp <- compileNimble(dInvGaussMixGPD)
#' dSp(3.0, w = w, mean = mu, shape = lam, u = u, sigma = 1, xi = 0.2)
#' qInvGaussMixGPD(0.99, w = w, mean = mu, shape = lam, u = u, sigma = 1, xi = 0.2)
#' }
#'
#' @rdname invgauss_mixgpd
#' @name invgauss_mixgpd
#' @aliases dInvGaussMixGPD pInvGaussMixGPD rInvGaussMixGPD qInvGaussMixGPD
NULL



#' @describeIn invgauss_mix Inverse Gaussian mixture density
#' @export
dInvGaussMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 mu = double(1, default = numeric(0)),
                 lambda = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(mean) == 0) mean <- param1
    if (length(shape) == 0) shape <- param2
    if (length(mean) == 0 & length(mu) > 0) mean <- mu
    if (length(shape) == 0 & length(lambda) > 0) shape <- lambda
    K <- length(w)
    s0 <- 0.0
    for (j in 1:K) s0 <- s0 + w[j] * dinvgauss(x, mean[j], shape[j], 0)
    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0)) else return(s0)
  }
)

#' @describeIn invgauss_mix Distribution function for Inverse Gaussian mixture
#' @export
pInvGaussMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 mu = double(1, default = numeric(0)),
                 lambda = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(mean) == 0) mean <- param1
    if (length(shape) == 0) shape <- param2
    if (length(mean) == 0 & length(mu) > 0) mean <- mu
    if (length(shape) == 0 & length(lambda) > 0) shape <- lambda
    K <- length(w)
    cdf <- 0.0
    for (j in 1:K) cdf <- cdf + w[j] * pinvgauss(q, mean[j], shape[j], 1, 0)
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn invgauss_mix Random generation for Inverse Gaussian mixture
#' @export
rInvGaussMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 mu = double(1, default = numeric(0)),
                 lambda = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1)) {
    returnType(double(0))
    if (length(mean) == 0) mean <- param1
    if (length(shape) == 0) shape <- param2
    if (length(mean) == 0 & length(mu) > 0) mean <- mu
    if (length(shape) == 0 & length(lambda) > 0) shape <- lambda
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
    return(rinvgauss(1, mean[idx], shape[idx]))
  }
)

#' @describeIn invgauss_mix Quantile function for Inverse Gaussian mixture
#' @export
qInvGaussMix <- function(p, w, mean, shape, mu = NULL, lambda = NULL,
                         param1 = mean, param2 = shape,
                         lower.tail = TRUE, log.p = FALSE,
                         tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(mean) == 0) mean <- as.numeric(param1)
  if (length(shape) == 0) shape <- as.numeric(param2)
  if (length(mean) == 0 & !is.null(mu)) mean <- as.numeric(mu)
  if (length(shape) == 0 & !is.null(lambda)) shape <- as.numeric(lambda)

  # mixture CDF using helper CDF in R
  pinv <- function(q, mu, lam) {
    z1 <- sqrt(lam/q) * (q/mu - 1)
    z2 <- -sqrt(lam/q) * (q/mu + 1)
    pnorm(z1) + exp(2*lam/mu) * pnorm(z2)
  }
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- 0; next }
    if (pi >= 1) { out[i] <- Inf; next }
    out[i] <- stats::uniroot(function(z) pInvGaussMix(z,w,mean,shape) - pi, interval = c(-1,1e10), tol = tol, maxiter = maxiter)$root
  }
  out
}

#' @describeIn invgauss_mixgpd Inverse Gaussian mixture + GPD tail density
#' @export
dInvGaussMixGPD <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 mu = double(1, default = numeric(0)),
                 lambda = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (x < u) return(dInvGaussMix(x, w, mean, shape, mu, lambda, param1, param2, log))
    Fu <- pInvGaussMix(u, w, mean, shape, mu, lambda, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    val <- (1.0 - Fu) * dGpd(x, u, sigma, xi, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn invgauss_mixgpd Inverse Gaussian mixture + GPD tail distribution function
#' @export
pInvGaussMixGPD <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 mu = double(1, default = numeric(0)),
                 lambda = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (q < u) return(pInvGaussMix(q, w, mean, shape, mu, lambda, param1, param2, lower.tail, log.p))
    Fu <- pInvGaussMix(u, w, mean, shape, mu, lambda, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    G <- pGpd(q, u, sigma, xi, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn invgauss_mixgpd Inverse Gaussian mixture + GPD tail random generation
#' @export
rInvGaussMixGPD <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 mean = double(1),
                 shape = double(1),
                 mu = double(1, default = numeric(0)),
                 lambda = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0)) {
    returnType(double(0))
    if (n != 1) return(0.0)
    Fu <- pInvGaussMix(u, w, mean, shape, mu, lambda, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rInvGaussMix(1, w, mean, shape, mu, lambda, param1, param2))
    return(rGpd(1, u, sigma, xi))
  }
)

#' @describeIn invgauss_mixgpd Inverse Gaussian mixture + GPD tail quantile function
#' @export
qInvGaussMixGPD <- function(p, w, mean, shape, u, sigma, xi,
                            mu = NULL, lambda = NULL, param1 = mean, param2 = shape,
                            lower.tail = TRUE, log.p = FALSE,
                            tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(mean) == 0) mean <- as.numeric(param1)
  if (length(shape) == 0) shape <- as.numeric(param2)
  if (length(mean) == 0 & !is.null(mu)) mean <- as.numeric(mu)
  if (length(shape) == 0 & !is.null(lambda)) shape <- as.numeric(lambda)

  pinv <- function(q, mu, lam) {
    z1 <- sqrt(lam/q) * (q/mu - 1)
    z2 <- -sqrt(lam/q) * (q/mu + 1)
    pnorm(z1) + exp(2*lam/mu) * pnorm(z2)
  }
  Fu <- sum(w * pinv(u, mean, shape))
  Fu <- max(min(Fu, 1.0), 0.0)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) out[i] <- qInvGaussMix(pi, w, mean, shape, lower.tail = TRUE, log.p = FALSE, tol = tol, maxiter = maxiter)
    else {
      g <- if (Fu >= 1) 0 else (pi - Fu)/(1 - Fu)
      out[i] <- qGpd(g, u = u, sigma = sigma, xi = xi)
    }
  }
  out
}




