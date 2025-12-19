#' Lognormal mixture distribution
#'
#' A finite mixture of Lognormal components. Base Lognormal functions are taken from \pkg{stats}.
#' Mixture density and CDF are computed by weighted sums. Random generation samples a component
#' index according to weights and draws from the corresponding component. Quantiles are computed
#' by numerical inversion of the mixture CDF.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w}
#'   internally when needed.
#' @param meanlog,sdlog Numeric vectors of length \eqn{K} giving component log-means and log-standard deviations.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Density/CDF/RNG functions return numeric scalars. \code{qLognormalMix} returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @examples
#' w <- c(0.6, 0.4)
#' meanlog <- c(0, 1)
#' sdlog <- c(0.5, 0.25)
#' dLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog, log = 0)
#' pLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog, lower.tail = 1, log.p = 0)
#' qLognormalMix(0.9, w = w, meanlog = meanlog, sdlog = sdlog)
#' rLognormalMix(1, w = w, meanlog = meanlog, sdlog = sdlog)
#'
#' @rdname lognormal_mix
#' @name lognormal_mix
#' @aliases dLognormalMix pLognormalMix rLognormalMix qLognormalMix
#' @importFrom stats dlnorm plnorm rlnorm qlnorm runif uniroot
NULL

#' @describeIn lognormal_mix Lognormal mixture density
#' @export
dLognormalMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    K <- length(w)
    wsum <- sum(w)
    if (wsum <= 0) {
      if (log == 1) return(-Inf) else return(0.0)
    }

    s0 <- 0.0
    for (j in 1:K) {
      s0 <- s0 + (w[j] / wsum) * dlnorm(x, meanlog[j], sdlog[j], 0)
    }
    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0)) else return(s0)
  }
)

#' @describeIn lognormal_mix Lognormal mixture distribution function
#' @export
pLognormalMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    K <- length(w)
    wsum <- sum(w)
    if (wsum <= 0) {
      if (log.p == 1) return(-Inf) else return(0.0)
    }

    cdf <- 0.0
    for (j in 1:K) {
      cdf <- cdf + (w[j] / wsum) * plnorm(q, meanlog[j], sdlog[j], 1, 0)
    }
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn lognormal_mix Lognormal mixture random generation
#' @export
rLognormalMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1)) {
    returnType(double(0))

    if (n != 1) return(0.0)
    K <- length(w)
    wsum <- sum(w)
    if (wsum <= 0) return(0.0)

    thresholdU <- runif(1, 0.0, wsum)
    cw <- 0.0
    idx <- 1
    found <- 0
    for (j in 1:K) {
      cw <- cw + w[j]
      if (found == 0) {
        if (thresholdU <= cw) {
          idx <- j
          found <- 1
        }
      }
    }
    return(rlnorm(1, meanlog[idx], sdlog[idx]))
  }
)

#' @describeIn lognormal_mix Lognormal mixture quantile function
#' @export
qLognormalMix <- function(p, w, meanlog, sdlog,
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
      function(z) as.numeric(pLognormalMix(z, w = w, meanlog = meanlog, sdlog = sdlog, 1, 0)) - pi,
      interval = c(0, 1e20),
      tol = tol, maxiter = maxiter
    )$root
  }
  out
}

#' Lognormal with a Gpd tail
#'
#' Splices a generalized Pareto distribution (Gpd) above \code{threshold} onto a single Lognormal bulk with
#' parameters \code{meanlog} and \code{sdlog}. Base Lognormal functions are taken from \pkg{stats}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param meanlog Numeric scalar log-mean parameter for the Lognormal bulk.
#' @param sdlog Numeric scalar log-standard deviation for the Lognormal bulk.
#' @param threshold Numeric scalar threshold at which the Gpd tail is attached.
#' @param tail_scale Numeric scalar Gpd scale parameter; must be positive.
#' @param tail_shape Numeric scalar Gpd shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qLognormalGpd} returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @rdname lognormal_gpd
#' @name lognormal_gpd
#' @aliases dLognormalGpd pLognormalGpd rLognormalGpd qLognormalGpd
NULL

#' @describeIn lognormal_gpd Lognormal + Gpd tail density
#' @export
dLognormalGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 meanlog = double(0),
                 sdlog = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (x < threshold) {
      if (log == 1) return(dlnorm(x, meanlog, sdlog, 1)) else return(dlnorm(x, meanlog, sdlog, 0))
    }

    Fu <- plnorm(threshold, meanlog, sdlog, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn lognormal_gpd Lognormal + Gpd tail distribution function
#' @export
pLognormalGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 meanlog = double(0),
                 sdlog = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (q < threshold) return(plnorm(q, meanlog, sdlog, lower.tail, log.p))

    Fu <- plnorm(threshold, meanlog, sdlog, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    G <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    cdf <- max(min(cdf, 1.0), 0.0)

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn lognormal_gpd Lognormal + Gpd tail random generation
#' @export
rLognormalGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 meanlog = double(0),
                 sdlog = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- plnorm(threshold, meanlog, sdlog, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rlnorm(1, meanlog, sdlog))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)

#' @describeIn lognormal_gpd Lognormal + Gpd tail quantile function
#' @export
qLognormalGpd <- function(p, meanlog, sdlog, threshold, tail_scale, tail_shape,
                          lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- stats::plnorm(threshold, meanlog = meanlog, sdlog = sdlog, lower.tail = TRUE, log.p = FALSE)
  Fu <- max(min(Fu, 1.0), 0.0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- stats::qlnorm(pi, meanlog = meanlog, sdlog = sdlog, lower.tail = TRUE, log.p = FALSE)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}


#' Lognormal mixture with a Gpd tail
#'
#' Splices a generalized Pareto distribution (Gpd) above \code{threshold} onto a Lognormal mixture bulk.
#' Let \eqn{F_{mix}} be the Lognormal mixture CDF. The spliced CDF is
#' \eqn{F(x)=F_{mix}(x)} for \eqn{x<threshold} and
#' \eqn{F(x)=F_{mix}(threshold) + \{1-F_{mix}(threshold)\}G(x)} for \eqn{x\ge threshold}, where \eqn{G}
#' is the Gpd CDF for exceedances above \code{threshold}.
#'
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s. The quantile is an R function:
#' it uses numerical inversion in the bulk region and the closed-form Gpd quantile in the tail.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param meanlog,sdlog Numeric vectors of length \eqn{K} giving component log-means and log-standard deviations.
#' @param threshold Numeric scalar threshold at which the Gpd tail is attached.
#' @param tail_scale Numeric scalar Gpd scale parameter; must be positive.
#' @param tail_shape Numeric scalar Gpd shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars.
#'   \code{qLognormalMixGpd} returns a numeric vector with the same length as \code{p}.
#'
#' @examples
#' w <- c(0.6, 0.4)
#' meanlog <- c(0, 1)
#' sdlog <- c(0.5, 0.25)
#' threshold <- 2
#' tail_scale <- 1.0
#' tail_shape <- 0.2
#' dLognormalMixGpd(3.0, w = w, meanlog = meanlog, sdlog = sdlog,
#'                  threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape, log = 0)
#' pLognormalMixGpd(3.0, w = w, meanlog = meanlog, sdlog = sdlog,
#'                  threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape,
#'                  lower.tail = 1, log.p = 0)
#' rLognormalMixGpd(1, w = w, meanlog = meanlog, sdlog = sdlog,
#'                  threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#' qLognormalMixGpd(0.9, w = w, meanlog = meanlog, sdlog = sdlog,
#'                  threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#'
#' @rdname lognormal_mixgpd
#' @name lognormal_mixgpd
#' @aliases dLognormalMixGpd pLognormalMixGpd rLognormalMixGpd qLognormalMixGpd
NULL

#' @describeIn lognormal_mixgpd Lognormal mixture + Gpd tail density
#' @export
dLognormalMixGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (x < threshold) return(dLognormalMix(x, w, meanlog, sdlog, log))

    Fu <- pLognormalMix(threshold, w, meanlog, sdlog, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn lognormal_mixgpd Lognormal mixture + Gpd tail distribution function
#' @export
pLognormalMixGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (q < threshold) return(pLognormalMix(q, w, meanlog, sdlog, lower.tail, log.p))

    Fu <- pLognormalMix(threshold, w, meanlog, sdlog, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    G <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    cdf <- max(min(cdf, 1.0), 0.0)

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn lognormal_mixgpd Lognormal mixture + Gpd tail random generation
#' @export
rLognormalMixGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 meanlog = double(1),
                 sdlog = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pLognormalMix(threshold, w, meanlog, sdlog, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)

    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rLognormalMix(1, w, meanlog, sdlog))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)

#' @describeIn lognormal_mixgpd Lognormal mixture + Gpd tail quantile function
#' @export
qLognormalMixGpd <- function(p, w, meanlog, sdlog, threshold, tail_scale, tail_shape,
                             lower.tail = TRUE, log.p = FALSE,
                             tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- as.numeric(pLognormalMix(threshold, w = w, meanlog = meanlog, sdlog = sdlog, 1, 0))
  Fu <- max(min(Fu, 1.0), 0.0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- qLognormalMix(pi, w, meanlog, sdlog,
                              lower.tail = TRUE, log.p = FALSE,
                              tol = tol, maxiter = maxiter)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}
