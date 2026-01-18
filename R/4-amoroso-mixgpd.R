#' Amoroso mixture distribution
#'
#' A finite mixture of Amoroso components is often a convenient bulk model when the response is
#' positive and right-skewed. These functions evaluate and simulate from
#' \eqn{\sum_j w_j f_{Amoroso}(\cdot\mid loc_j, scale_j, shape1_j, shape2_j)}.
#'
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s so they can be called from
#' NIMBLE models. The quantile function is provided as an R function and is computed by numerical
#' inversion of the mixture CDF.
#' The \code{d*}, \code{p*}, and \code{q*} functions accept vector inputs for their first argument
#' and evaluate elementwise; \code{r*} supports \code{n > 1}.
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
#' @param tol Numeric tolerance for numerical inversion in \code{qAmorosoGpd}.
#' @param maxiter Maximum iterations for numerical inversion in \code{qAmorosoGpd}.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Mixture density/CDF/RNG functions return numeric scalars. \code{qAmorosoMix} returns a numeric
#'   vector with the same length as \code{p}.
#'
#' @examples
#' w <- c(0.60, 0.25, 0.15)
#' loc <- c(0, 1, 2)
#' scale <- c(1.0, 1.2, 1.6)
#' shape1 <- c(2, 4, 6)
#' shape2 <- c(1.0, 1.2, 1.5)
#'
#' dAmorosoMix(2.0, w, loc, scale, shape1, shape2, log = 0)
#' pAmorosoMix(2.0, w, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)
#' qAmorosoMix(0.50, w, loc, scale, shape1, shape2)
#' qAmorosoMix(0.95, w, loc, scale, shape1, shape2)
#' replicate(10, rAmorosoMix(1, w, loc, scale, shape1, shape2))
#' @rdname amoroso_mix
#' @name amoroso_mix
#' @aliases dAmorosoMix pAmorosoMix rAmorosoMix qAmorosoMix
NULL


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
    wsum <- sum(w)
    if (wsum <= 0.0) {
      if (log == 1) return(-1.0e300) else return(0.0)
    }
    ww <- w / wsum

    s0 <- 0.0
    for (j in 1:K) {
      s0 <- s0 + ww[j] * dAmoroso(x, loc[j], scale[j], shape1[j], shape2[j], 0)
    }
    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0)) else return(s0)
  }
)

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
    wsum <- sum(w)
    if (wsum <= 0.0) {
      if (log.p != 0) return(-1.0e300) else return(0.0)
    }
    ww <- w / wsum

    cdf <- 0.0
    for (j in 1:K) {
      cdf <- cdf + ww[j] * pAmoroso(q, loc[j], scale[j], shape1[j], shape2[j], 1, 0)
    }

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) return(log(max(cdf, eps)))
    return(cdf)
  }
)

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

    return(rAmoroso(1, loc[idx], scale[idx], shape1[idx], shape2[idx]))
  }
)

#' @describeIn amoroso_mix Quantile Function of Amoroso Mixture Distribution
#' @export
qAmorosoMix <- function(p, w, loc, scale, shape1, shape2,
                        lower.tail = TRUE, log.p = FALSE,
                        tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- -Inf; next }
    if (pi >= 1) { out[i] <- Inf; next }

    lo <- min(qAmoroso(pi, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE), na.rm = TRUE)
    hi <- max(qAmoroso(pi, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE), na.rm = TRUE)
    if (!is.finite(lo)) lo <- -1e10
    if (!is.finite(hi)) hi <- 1e10
    if (lo > hi) {
      tmp <- lo
      lo <- hi
      hi <- tmp
    }
    if (lo == hi) {
      lo <- lo - 1
      hi <- hi + 1
    }
    f_lo <- as.numeric(pAmorosoMix(lo, w, loc, scale, shape1, shape2, 1, 0) - pi)
    f_hi <- as.numeric(pAmorosoMix(hi, w, loc, scale, shape1, shape2, 1, 0) - pi)
    iter <- 0L
    while (is.finite(f_lo) && f_lo > 0 && lo > -1e20 && iter < 60L) {
      step <- max(1, abs(lo))
      lo <- lo - step
      f_lo <- as.numeric(pAmorosoMix(lo, w, loc, scale, shape1, shape2, 1, 0) - pi)
      iter <- iter + 1L
    }
    iter <- 0L
    while (is.finite(f_hi) && f_hi < 0 && hi < 1e20 && iter < 60L) {
      step <- max(1, abs(hi))
      hi <- hi + step
      f_hi <- as.numeric(pAmorosoMix(hi, w, loc, scale, shape1, shape2, 1, 0) - pi)
      iter <- iter + 1L
    }
    if (!is.finite(lo) || !is.finite(hi) || lo >= hi || !is.finite(f_lo) || !is.finite(f_hi) || f_lo * f_hi > 0) {
      out[i] <- NA_real_
    } else {
      out[i] <- stats::uniroot(
        function(z) pAmorosoMix(z, w, loc, scale, shape1, shape2, 1, 0) - pi,
        interval = c(lo, hi),
        tol = tol, maxiter = maxiter
      )$root
    }
  }
  out
}


#' Amoroso mixture with a GPD tail
#'
#' This family splices a generalized Pareto distribution (GPD) above a threshold \code{threshold} onto an
#' Amoroso mixture bulk. Let \eqn{F_{mix}} denote the Amoroso mixture CDF. The spliced CDF is
#' \eqn{F(x)=F_{mix}(x)} for \eqn{x<threshold} and
#' \eqn{F(x)=F_{mix}(threshold) + \left\{1-F_{mix}(threshold)\right\}G(x)} for \eqn{x\ge threshold}, where \eqn{G}
#' is the GPD CDF for exceedances above \code{threshold}.
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
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric tolerance for numerical inversion in \code{qAmorosoGpd}.
#' @param maxiter Maximum iterations for numerical inversion in \code{qAmorosoGpd}.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qAmorosoMixGpd} returns a numeric
#'   vector with the same length as \code{p}.
#'
#' @examples
#' w <- c(0.60, 0.25, 0.15)
#' loc <- c(0, 1, 2)
#' scale <- c(1.0, 1.2, 1.6)
#' shape1 <- c(2, 4, 6)
#' shape2 <- c(1.0, 1.2, 1.5)
#' threshold <- 3
#' tail_scale <- 1.0
#' tail_shape <- 0.2
#'
#' dAmorosoMixGpd(4.0, w, loc, scale, shape1, shape2,
#'               threshold, tail_scale, tail_shape, log = 0)
#' pAmorosoMixGpd(4.0, w, loc, scale, shape1, shape2,
#'               threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
#' qAmorosoMixGpd(0.50, w, loc, scale, shape1, shape2,
#'               threshold, tail_scale, tail_shape)
#' qAmorosoMixGpd(0.95, w, loc, scale, shape1, shape2,
#'               threshold, tail_scale, tail_shape)
#' replicate(10, rAmorosoMixGpd(1, w, loc, scale, shape1, shape2,
#'                             threshold, tail_scale, tail_shape))
#' @rdname amoroso_mixgpd
#' @name amoroso_mixgpd
#' @aliases dAmorosoMixGpd pAmorosoMixGpd rAmorosoMixGpd qAmorosoMixGpd
NULL



#' @describeIn amoroso_mixgpd Density Function of Amoroso Mixture Distribution with GPD Tail
#' @export
dAmorosoMixGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 loc = double(1),
                 scale = double(1),
                 shape1 = double(1),
                 shape2 = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    eps <- 1e-300
    if (x < threshold) return(dAmorosoMix(x, w, loc, scale, shape1, shape2, log))

    Fu <- pAmorosoMix(threshold, w, loc, scale, shape1, shape2, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)
    if (val < eps) val <- eps

    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn amoroso_mixgpd Cumulative Distribution Function of Amoroso Mixture Distribution with GPD Tail
#' @export
pAmorosoMixGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 loc = double(1),
                 scale = double(1),
                 shape1 = double(1),
                 shape2 = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    eps <- 1e-300
    if (q < threshold) return(pAmorosoMix(q, w, loc, scale, shape1, shape2, lower.tail, log.p))

    Fu <- pAmorosoMix(threshold, w, loc, scale, shape1, shape2, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    G <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) return(log(max(cdf, eps)))
    return(cdf)
  }
)

#' @describeIn amoroso_mixgpd Random Generation for Amoroso Mixture Distribution with GPD Tail
#' @export
rAmorosoMixGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 loc = double(1),
                 scale = double(1),
                 shape1 = double(1),
                 shape2 = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pAmorosoMix(threshold, w, loc, scale, shape1, shape2, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rAmorosoMix(1, w, loc, scale, shape1, shape2))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)

#' @describeIn amoroso_mixgpd Quantile Function of Amoroso Mixture Distribution with GPD Tail
#' @export
qAmorosoMixGpd <- function(p, w, loc, scale, shape1, shape2,
                           threshold, tail_scale, tail_shape,
                           lower.tail = TRUE, log.p = FALSE,
                           tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)

  # Fu computed using the compiled mixture CDF at threshold (consistent with PD/R definitions)
  Fu <- pAmorosoMix(threshold, w, loc, scale, shape1, shape2, 1, 0)
  Fu <- max(min(as.numeric(Fu), 1), 0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- qAmorosoMix(pi, w, loc, scale, shape1, shape2,
                            lower.tail = TRUE, log.p = FALSE, tol = tol, maxiter = maxiter)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}


#' Amoroso with a GPD tail
#'
#' Splices a generalized Pareto distribution (GPD) above a threshold \code{threshold} onto a single Amoroso bulk
#' with parameters \code{loc}, \code{scale}, \code{shape1}, and \code{shape2}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param loc Numeric scalar location parameter of the Amoroso bulk.
#' @param scale Numeric scalar scale parameter of the Amoroso bulk.
#' @param shape1 Numeric scalar first Amoroso shape parameter.
#' @param shape2 Numeric scalar second Amoroso shape parameter.
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qAmorosoGpd} returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @examples
#' loc <- 0
#' scale <- 1.5
#' shape1 <- 2
#' shape2 <- 1.2
#' threshold <- 3
#' tail_scale <- 1.0
#' tail_shape <- 0.2
#'
#' dAmorosoGpd(4.0, loc, scale, shape1, shape2,
#'            threshold, tail_scale, tail_shape, log = 0)
#' pAmorosoGpd(4.0, loc, scale, shape1, shape2,
#'            threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
#' qAmorosoGpd(0.50, loc, scale, shape1, shape2,
#'            threshold, tail_scale, tail_shape)
#' qAmorosoGpd(0.95, loc, scale, shape1, shape2,
#'            threshold, tail_scale, tail_shape)
#' replicate(10, rAmorosoGpd(1, loc, scale, shape1, shape2,
#'                          threshold, tail_scale, tail_shape))
#' @rdname amoroso_gpd
#' @name amoroso_gpd
#' @aliases dAmorosoGpd pAmorosoGpd rAmorosoGpd qAmorosoGpd
NULL

#' @describeIn amoroso_gpd Density Function of Amoroso Distribution with GPD Tail
#' @export
dAmorosoGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 loc = double(0),
                 scale = double(0),
                 shape1 = double(0),
                 shape2 = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))

    eps <- 1e-300
    if (x < threshold) return(dAmoroso(x, loc, scale, shape1, shape2, log))

    Fu <- pAmoroso(threshold, loc, scale, shape1, shape2, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)
    if (val < eps) val <- eps

    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn amoroso_gpd Cumulative Distribution Function of Amoroso Distribution with GPD Tail
#' @export
pAmorosoGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 loc = double(0),
                 scale = double(0),
                 shape1 = double(0),
                 shape2 = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))

    eps <- 1e-300
    if (q < threshold) return(pAmoroso(q, loc, scale, shape1, shape2, lower.tail, log.p))

    Fu <- pAmoroso(threshold, loc, scale, shape1, shape2, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    G <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0

    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) return(log(max(cdf, eps)))
    return(cdf)
  }
)

#' @describeIn amoroso_gpd Random Generation for Amoroso Distribution with GPD Tail
#' @export
rAmorosoGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 loc = double(0),
                 scale = double(0),
                 shape1 = double(0),
                 shape2 = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pAmoroso(threshold, loc, scale, shape1, shape2, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rAmoroso(1, loc, scale, shape1, shape2))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)

#' @describeIn amoroso_gpd Quantile Function of Amoroso Distribution with GPD Tail
#' @export
#' @param tol Numeric tolerance for numerical inversion in \code{qAmorosoGpd}.
#' @param maxiter Maximum iterations for numerical inversion in \code{qAmorosoGpd}.
qAmorosoGpd <- function(p, loc, scale, shape1, shape2,
                        threshold, tail_scale, tail_shape,
                        lower.tail = TRUE, log.p = FALSE,
                        tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- pAmoroso(threshold, loc, scale, shape1, shape2, 1, 0)
  Fu <- max(min(as.numeric(Fu), 1), 0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      # bulk quantile via numerical inversion of base CDF
      lo <- qAmoroso(pi, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE)
      hi <- lo
      if (!is.finite(lo)) {
        lo <- -1e10
        hi <- 1e10
      }
      if (lo > hi) {
        tmp <- lo
        lo <- hi
        hi <- tmp
      }
      if (lo == hi) {
        lo <- lo - 1
        hi <- hi + 1
      }
      f_lo <- as.numeric(pAmoroso(lo, loc, scale, shape1, shape2, 1, 0) - pi)
      f_hi <- as.numeric(pAmoroso(hi, loc, scale, shape1, shape2, 1, 0) - pi)
      iter <- 0L
      while (is.finite(f_lo) && f_lo > 0 && lo > -1e20 && iter < 60L) {
        step <- max(1, abs(lo))
        lo <- lo - step
        f_lo <- as.numeric(pAmoroso(lo, loc, scale, shape1, shape2, 1, 0) - pi)
        iter <- iter + 1L
      }
      iter <- 0L
      while (is.finite(f_hi) && f_hi < 0 && hi < 1e20 && iter < 60L) {
        step <- max(1, abs(hi))
        hi <- hi + step
        f_hi <- as.numeric(pAmoroso(hi, loc, scale, shape1, shape2, 1, 0) - pi)
        iter <- iter + 1L
      }
      if (!is.finite(lo) || !is.finite(hi) || lo >= hi || !is.finite(f_lo) || !is.finite(f_hi) || f_lo * f_hi > 0) {
        out[i] <- NA_real_
      } else {
        out[i] <- stats::uniroot(
          function(z) pAmoroso(z, loc, scale, shape1, shape2, 1, 0) - pi,
          interval = c(lo, hi),
          tol = tol, maxiter = maxiter
        )$root
      }
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}

