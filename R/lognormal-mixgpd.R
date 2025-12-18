#' Lognormal mixture distribution
#'
#' This topic documents the finite-mixture version of the distribution.
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s so they can be used
#' inside NIMBLE models. The quantile function is an R function computed by numerical inversion
#' of the mixture CDF.
#'
#' Each component \eqn{j} is a lognormal distribution with log-scale parameters \code{meanlog[j]} and \code{sdlog[j]}.
#' For convenience, you may instead provide \code{mean} and \code{sd} on the original scale; these are converted
#' internally to the corresponding \code{meanlog} and \code{sdlog} values.
#' The generic aliases \code{param1} and \code{param2} are accepted for \code{meanlog} and \code{sdlog}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE, RNG supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w} internally when needed.
#' @param meanlog,sdlog Numeric vectors of length \eqn{K} giving component log-means and log-standard deviations.
#' @param mean,sd Numeric vectors of length \eqn{K} giving component means and standard deviations on the original scale.
#'   If supplied, both must be provided and will be converted to log-scale parameters.
#' @param param1,param2 Generic aliases for \code{meanlog} and \code{sdlog}.
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
#' w <- c(0.7, 0.3)
#' ml <- c(0, 1)
#' sl <- c(0.5, 0.2)
#' dM <- compileNimble(dLognormalMix)
#' dM(1.0, w = w, meanlog = ml, sdlog = sl)
#' qLognormalMix(0.9, w = w, meanlog = ml, sdlog = sl)
#' }
#'
#' @rdname lognormal_mix
#' @name lognormal_mix
#' @aliases dLognormalMix pLognormalMix rLognormalMix qLognormalMix
NULL


#' Lognormal mixture with a GPD tail
#'
#' This topic documents the mixture distribution spliced with a generalized Pareto (GPD) tail above
#' a threshold \code{u}. The bulk mixture governs \eqn{x<u}. For \eqn{x\ge u}, exceedances follow a GPD
#' and are weighted by \eqn{1 - F_{mix}(u)} so that the overall distribution remains proper.
#'
#' The bulk distribution is a lognormal mixture. A generalized Pareto tail is attached above \code{u}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE, RNG supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param meanlog,sdlog Numeric vectors of length \eqn{K} giving component log-means and log-standard deviations.
#' @param mean,sd Numeric vectors of length \eqn{K} giving component means and standard deviations on the original scale.
#' @param param1,param2 Generic aliases for \code{meanlog} and \code{sdlog}.
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
#' w <- c(0.7, 0.3)
#' ml <- c(0, 1)
#' sl <- c(0.5, 0.2)
#' u <- 2
#' dSp <- compileNimble(dLognormalMixGPD)
#' dSp(3.0, w = w, meanlog = ml, sdlog = sl, u = u, sigma = 1, xi = 0.2)
#' qLognormalMixGPD(0.99, w = w, meanlog = ml, sdlog = sl, u = u, sigma = 1, xi = 0.2)
#' }
#'
#' @rdname lognormal_mixgpd
#' @name lognormal_mixgpd
#' @aliases dLognormalMixGPD pLognormalMixGPD rLognormalMixGPD qLognormalMixGPD
NULL
.to_logpar <- function(mean, sd) {
  v <- sd^2
  meanlog <- log(mean^2 / sqrt(v + mean^2))
  sdlog <- sqrt(log(1 + v/mean^2))
  list(meanlog = meanlog, sdlog = sdlog)
}

#' @describeIn lognormal_mix Density function for Lognormal mixture
#' @export
dLognormalMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 mean = double(1, default = numeric(0)),
                 sd = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(meanlog) == 0) meanlog <- param1
    if (length(sdlog) == 0) sdlog <- param2
    # allow original-scale mean/sd if provided
    if (length(meanlog) == 0 & length(mean) > 0 & length(sd) > 0) {
      # compute elementwise; NIMBLE doesn't like list return, so do inline in loops
      meanlog <- log(mean^2 / sqrt(sd^2 + mean^2))
      sdlog <- sqrt(log(1 + (sd^2)/(mean^2)))
    }
    K <- length(w)
    s0 <- 0.0
    for (j in 1:K) s0 <- s0 + w[j] * dlnorm(x, meanlog = meanlog[j], sdlog = sdlog[j], log = 0)
    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0)) else return(s0)
  }
)

#' @describeIn lognormal_mix Cumulative distribution function for Lognormal mixture
#' @export
pLognormalMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 mean = double(1, default = numeric(0)),
                 sd = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(meanlog) == 0) meanlog <- param1
    if (length(sdlog) == 0) sdlog <- param2
    if (length(meanlog) == 0 & length(mean) > 0 & length(sd) > 0) {
      meanlog <- log(mean^2 / sqrt(sd^2 + mean^2))
      sdlog <- sqrt(log(1 + (sd^2)/(mean^2)))
    }
    K <- length(w)
    cdf <- 0.0
    for (j in 1:K) cdf <- cdf + w[j] * plnorm(q, meanlog = meanlog[j], sdlog = sdlog[j], lower.tail = 1, log.p = 0)
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn lognormal_mix Random generation for Lognormal mixture
#' @export
rLognormalMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 mean = double(1, default = numeric(0)),
                 sd = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1)) {
    returnType(double(0))
    if (length(meanlog) == 0) meanlog <- param1
    if (length(sdlog) == 0) sdlog <- param2
    if (length(meanlog) == 0 & length(mean) > 0 & length(sd) > 0) {
      meanlog <- log(mean^2 / sqrt(sd^2 + mean^2))
      sdlog <- sqrt(log(1 + (sd^2)/(mean^2)))
    }
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
    return(rlnorm(1, meanlog = meanlog[idx], sdlog = sdlog[idx]))
  }
)

#' @describeIn lognormal_mix Quantile function for Lognormal mixture
#' @export
qLognormalMix <- function(p, w, meanlog, sdlog, mean = NULL, sd = NULL,
                          param1 = meanlog, param2 = sdlog,
                          lower.tail = TRUE, log.p = FALSE,
                          tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(meanlog) == 0) meanlog <- as.numeric(param1)
  if (length(sdlog) == 0) sdlog <- as.numeric(param2)
  if (length(meanlog) == 0 & !is.null(mean) & !is.null(sd)) {
    tmp <- .to_logpar(as.numeric(mean), as.numeric(sd))
    meanlog <- tmp$meanlog; sdlog <- tmp$sdlog
  }

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- 0; next }
    if (pi >= 1) { out[i] <- Inf; next }

    out[i] <- stats::uniroot(function(z) pLognormalMix(z,w,meanlog ,sdlog) - pi, interval = c(-1,1e10), tol = tol, maxiter = maxiter)$root
  }
  out
}

#' @describeIn lognormal_mixgpd Density function for Lognormal mixture with GPD tail.
#' @export
dLognormalMixGPD <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 mean = double(1, default = numeric(0)),
                 sd = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (x < u) return(dLognormalMix(x, w, meanlog, sdlog, mean, sd, param1, param2, log))
    Fu <- pLognormalMix(u, w, meanlog, sdlog, mean, sd, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    val <- (1.0 - Fu) * dGpd(x, u, sigma, xi, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn lognormal_mixgpd Cumulative distribution function for Lognormal mixture with GPD tail.
#' @export
pLognormalMixGPD <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 mean = double(1, default = numeric(0)),
                 sd = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (q < u) return(pLognormalMix(q, w, meanlog, sdlog, mean, sd, param1, param2, lower.tail, log.p))
    Fu <- pLognormalMix(u, w, meanlog, sdlog, mean, sd, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    G <- pGpd(q, u, sigma, xi, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn lognormal_mixgpd Random generation for Lognormal mixture with GPD tail.
#' @export
rLognormalMixGPD <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 mean = double(1, default = numeric(0)),
                 sd = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0)) {
    returnType(double(0))
    if (n != 1) return(0.0)
    Fu <- pLognormalMix(u, w, meanlog, sdlog, mean, sd, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rLognormalMix(1, w, meanlog, sdlog, mean, sd, param1, param2))
    return(rGpd(1, u, sigma, xi))
  }
)

#' @describeIn lognormal_mixgpd Quantile function for Lognormal mixture with GPD tail.
#' @export
qLognormalMixGPD <- function(p, w, meanlog, sdlog, u, sigma, xi,
                             mean = NULL, sd = NULL, param1 = meanlog, param2 = sdlog,
                             lower.tail = TRUE, log.p = FALSE,
                             tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(meanlog) == 0) meanlog <- as.numeric(param1)
  if (length(sdlog) == 0) sdlog <- as.numeric(param2)
  if (length(meanlog) == 0 & !is.null(mean) & !is.null(sd)) {
    tmp <- .to_logpar(as.numeric(mean), as.numeric(sd))
    meanlog <- tmp$meanlog; sdlog <- tmp$sdlog
  }
  Fu <- sum(w * stats::plnorm(u, meanlog = meanlog, sdlog = sdlog))
  Fu <- max(min(Fu, 1.0), 0.0)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) out[i] <- qLognormalMix(pi, w, meanlog, sdlog, lower.tail = TRUE, log.p = FALSE, tol = tol, maxiter = maxiter)
    else {
      g <- if (Fu >= 1) 0 else (pi - Fu)/(1 - Fu)
      out[i] <- qGpd(g, u = u, sigma = sigma, xi = xi)
    }
  }
  out
}
