#' Laplace (double exponential) mixture distribution
#'
#' A finite mixture of Laplace (double exponential) components for real-valued data.
#' Base Laplace functions are taken from \pkg{nimble} (\code{ddexp}, \code{pdexp},
#' \code{rdexp}, \code{qdexp}).
#'
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
#' @param location,scale Numeric vectors of length \eqn{K} giving component locations and scales.
#' @param log Logical; if \code{TRUE}, return the log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param matail_shapeter Integer matail_shapelocationm number of iterations for \code{stats::uniroot}.
#'
#' @return Density/CDF/RNG functions return numeric scalars. \code{qLaplaceMix} returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @examples
#' w <- c(0.50, 0.30, 0.20)
#' location <- c(-1, 0.5, 2.0)
#' scale <- c(1.0, 0.7, 1.4)
#'
#' dLaplaceMix(0.8, w = w, location = location, scale = scale, log = FALSE)
#' pLaplaceMix(0.8, w = w, location = location, scale = scale,
#'            lower.tail = TRUE, log.p = FALSE)
#' qLaplaceMix(0.50, w = w, location = location, scale = scale)
#' qLaplaceMix(0.95, w = w, location = location, scale = scale)
#' replicate(10, rLaplaceMix(1, w = w, location = location, scale = scale))
#' @rdname laplace_mix
#' @name laplace_mix
#' @aliases dLaplaceMix pLaplaceMix rLaplaceMix qLaplaceMix
#' @importFrom stats runif uniroot
#' @importFrom nimble ddexp pdexp rdexp qdexp
NULL

#' @describeIn laplace_mix Laplace mixture density
#' @export
dLaplaceMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 location = double(1),
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
      s0 <- s0 + ww[j] * ddexp(x, location[j], scale[j], log = 0)
    }

    if (log == 1) return(log(s0))
    return(s0)
  }
)

#' @describeIn laplace_mix Laplace mixture distribution function
#' @export
pLaplaceMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 location = double(1),
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
      cdf <- cdf + ww[j] * pdexp(q, location[j], scale[j], lower.tail = 1, log.p = 0)
    }

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn laplace_mix Laplace mixture random generation
#' @export
rLaplaceMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 location = double(1),
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

    return(rdexp(1, location[idx], scale[idx]))
  }
)



#' @describeIn laplace_mix Laplace mixture quantile function
#' @export
qLaplaceMix <- function(p, w, location, scale,
                        lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- -Inf; next }
    if (pi >= 1) { out[i] <- Inf; next }

    out[i] <- stats::uniroot(
      function(z) pLaplaceMix(z, w = w,  location, scale,
                              lower.tail = TRUE, log.p = FALSE) - pi,
      interval = c(-1e20, 1e20)
    )$root
  }
  out
}


#' Laplace mixture with a Gpd tail
#'
#' Splices a generalized Pareto distribution (Gpd) above \code{threshold} onto a Laplace mixture bulk.
#' The bulk probability at the threshold is used to scale the tail so that the overall CDF is proper.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param location,scale Numeric vectors of length \eqn{K} giving component locations and scales.
#' @param threshold Numeric scalar threshold at which the Gpd tail is attached.
#' @param scale Numeric scalar Gpd scale parameter; locationst be positive.
#' @param tail_shape Numeric scalar Gpd shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param matail_shapeter Integer matail_shapelocationm number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qLaplaceMixGpd} returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @examples
#' w <- c(0.50, 0.30, 0.20)
#' location <- c(-1, 0.5, 2.0)
#' scale <- c(1.0, 0.7, 1.4)
#' threshold <- 1
#' tail_scale <- 1.0
#' tail_shape <- 0.2
#'
#' dLaplaceMixGpd(2.0, w = w, location = location, scale = scale,
#'               threshold = threshold, tail_scale = tail_scale,
#'               tail_shape = tail_shape, log = FALSE)
#' pLaplaceMixGpd(2.0, w = w, location = location, scale = scale,
#'               threshold = threshold, tail_scale = tail_scale,
#'               tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE)
#' qLaplaceMixGpd(0.50, w = w, location = location, scale = scale,
#'               threshold = threshold, tail_scale = tail_scale,
#'               tail_shape = tail_shape)
#' qLaplaceMixGpd(0.95, w = w, location = location, scale = scale,
#'               threshold = threshold, tail_scale = tail_scale,
#'               tail_shape = tail_shape)
#' replicate(10, rLaplaceMixGpd(1, w = w, location = location, scale = scale,
#'                             threshold = threshold,
#'                             tail_scale = tail_scale,
#'                             tail_shape = tail_shape))
#' @rdname laplace_MixGpd
#' @name laplace_MixGpd
#' @aliases dLaplaceMixGpd pLaplaceMixGpd rLaplaceMixGpd qLaplaceMixGpd
NULL

#' @describeIn laplace_MixGpd Laplace mixture + Gpd tail density
#' @export
dLaplaceMixGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    if (x < threshold) return(dLaplaceMix(x, w, location, scale, log))

    Fu <- pLaplaceMix(threshold, w, location, scale, 1, 0)
    val <- (1.0 - Fu) * dGpd(x, threshold, scale = tail_scale, shape = tail_shape, log = 0)

    if (log == 1) return(log(val))
    return(val)
  }
)

#' @describeIn laplace_MixGpd Laplace mixture + Gpd tail distribution function
#' @export
pLaplaceMixGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    # FIXED: below threshold must call pLaplaceMix(..., scale = scale), not tail_scale
    if (q < threshold) return(pLaplaceMix(q, w, location, scale, lower.tail, log.p))

    Fu <- pLaplaceMix(threshold, w, location, scale, 1, 0)
    G  <- pGpd(q, threshold, scale = tail_scale, shape = tail_shape, lower.tail = 1, log.p = 0)

    cdf <- Fu + (1.0 - Fu) * G

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn laplace_MixGpd Laplace mixture + Gpd tail random generation
#' @export
rLaplaceMixGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pLaplaceMix(threshold, w, location, scale, 1, 0)
    u  <- runif(1, 0.0, 1.0)

    if (u < Fu) return(rLaplaceMix(1, w, location, scale))
    return(rGpd(1, threshold, scale = tail_scale, shape = tail_shape))
  }
)



#' @describeIn laplace_MixGpd Laplace mixture + Gpd tail quantile function
#' @export
qLaplaceMixGpd <- function(p, w, location, scale, threshold, tail_scale, tail_shape,
                           lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- pLaplaceMix(threshold, w, location, scale, lower.tail = TRUE, log.p = FALSE)
  out <- numeric(length(p))

  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- qLaplaceMix(pi, w, location, scale, lower.tail = TRUE, log.p = FALSE)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g,  threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}


#' Laplace with a Gpd tail
#'
#' Splices a generalized Pareto distribution (Gpd) above \code{threshold} onto a single Laplace bulk with
#' parameters \code{location} and \code{scale}. Base Laplace functions are taken from \pkg{nimble}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param location Numeric scalar location parameter for the Laplace bulk.
#' @param scale Numeric scalar scale parameter for the Laplace bulk.
#' @param threshold Numeric scalar threshold at which the Gpd tail is attached.
#' @param scale Numeric scalar Gpd scale parameter; locationst be positive.
#' @param tail_shape Numeric scalar Gpd shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qLaplaceGpd} returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @examples
#' location <- 0.5
#' scale <- 1.0
#' threshold <- 1
#' tail_scale <- 1.0
#' tail_shape <- 0.2
#'
#' dLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape, log = FALSE)
#' pLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape,
#'            lower.tail = TRUE, log.p = FALSE)
#' qLaplaceGpd(0.50, location, scale, threshold, tail_scale, tail_shape)
#' qLaplaceGpd(0.95, location, scale, threshold, tail_scale, tail_shape)
#' replicate(10, rLaplaceGpd(1, location, scale, threshold, tail_scale, tail_shape))
#' @rdname laplace_gpd
#' @name laplace_gpd
#' @aliases dLaplaceGpd pLaplaceGpd rLaplaceGpd qLaplaceGpd
NULL

#' @describeIn laplace_gpd Laplace + Gpd tail density
#' @export
dLaplaceGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 location = double(0),
                 scale = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    if (x < threshold) return(ddexp(x, location, scale, log = log))

    Fu <- pdexp(threshold, location, scale, lower.tail = 1, log.p = 0)
    val <- (1.0 - Fu) * dGpd(x, threshold, scale = tail_scale, shape = tail_shape, log = 0)

    if (log == 1) return(log(val))
    return(val)
  }
)

#' @describeIn laplace_gpd Laplace + Gpd tail distribution function
#' @export
pLaplaceGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 location = double(0),
                 scale = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    if (q < threshold) return(pdexp(q, location, scale, lower.tail = lower.tail, log.p = log.p))

    Fu <- pdexp(threshold, location, scale, lower.tail = 1, log.p = 0)
    G  <- pGpd(q, threshold, scale = tail_scale, shape = tail_shape, lower.tail = 1, log.p = 0)

    cdf <- Fu + (1.0 - Fu) * G

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(cdf))
    return(cdf)
  }
)

#' @describeIn laplace_gpd Laplace + Gpd tail random generation
#' @export
rLaplaceGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 location = double(0),
                 scale = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pdexp(threshold, location, scale, lower.tail = 1, log.p = 0)
    u  <- runif(1, 0.0, 1.0)

    if (u < Fu) return(rdexp(1, location, scale))
    return(rGpd(1, threshold, scale = tail_scale, shape = tail_shape))
  }
)



#' @describeIn laplace_gpd Laplace + Gpd tail quantile function
#' @export
qLaplaceGpd <- function(p, location, scale, threshold, tail_scale, tail_shape, lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- nimble::pdexp(threshold,  location, scale, lower.tail = TRUE, log.p = FALSE)
  out <- numeric(length(p))

  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- nimble::qdexp(pi,  location, scale, lower.tail = TRUE, log.p = FALSE)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g,  threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}


