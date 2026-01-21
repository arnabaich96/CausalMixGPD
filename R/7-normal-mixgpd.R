#' Normal mixture distribution
#'
#' A finite mixture of Normal components. Base Normal functions are taken from \pkg{stats}.
#' Mixture density and CDF are computed by weighted sums. Random generation samples a component
#' index according to weights and draws from the corresponding component. Quantiles are computed
#' by numerical inversion of the mixture CDF.
#' The \code{d*}, \code{p*}, and \code{q*} functions accept vector inputs for their first argument
#' and evaluate elementwise; \code{r*} supports \code{n > 1}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w}
#'   internally when needed.
#' @param mean,sd Numeric vectors of length \eqn{K} giving component means and standard deviations.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Density/CDF/RNG functions return numeric scalars. \code{qNormMix} returns a numeric vector
#'   with the same length as \code{p}.
#'
#' @examples
#' w <- c(0.60, 0.25, 0.15)
#' mean <- c(-1, 0.5, 2.0)
#' sd <- c(1.0, 0.7, 1.3)
#'
#' dNormMix(0.5, w = w, mean = mean, sd = sd, log = FALSE)
#' pNormMix(0.5, w = w, mean = mean, sd = sd,
#'         lower.tail = TRUE, log.p = FALSE)
#' qNormMix(0.50, w = w, mean = mean, sd = sd)
#' qNormMix(0.95, w = w, mean = mean, sd = sd)
#' replicate(10, rNormMix(1, w = w, mean = mean, sd = sd))
#' @rdname normal_mix
#' @name normal_mix
#' @aliases dNormMix pNormMix rNormMix qNormMix
#' @importFrom stats uniroot pnorm dnorm runif qnorm rnorm
NULL

#' @describeIn normal_mix Normal mixture density
#' @export
dNormMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    K <- length(w)

    wsum <- 0.0
    for (j in 1:K) wsum <- wsum + w[j]
    if (wsum <= 0.0) {
      if (log == 1) return(log(eps)) else return(eps)
    }

    s0 <- 0.0
    for (j in 1:K) {
      s0 <- s0 + (w[j] / wsum) * dnorm(x, mean[j], sd[j], 0)
    }

    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0)) else return(s0)
  }
)

#' @describeIn normal_mix Normal mixture distribution function
#' @export
pNormMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    K <- length(w)

    wsum <- 0.0
    for (j in 1:K) wsum <- wsum + w[j]
    if (wsum <= 0.0) {
      cdf0 <- 0.0
      if (lower.tail == 0) cdf0 <- 1.0
      if (log.p == 1) return(log(max(cdf0, eps))) else return(cdf0)
    }

    cdf <- 0.0
    for (j in 1:K) {
      cdf <- cdf + (w[j] / wsum) * pnorm(q, mean[j], sd[j], 1, 0)
    }

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(max(cdf, eps))) else return(cdf)
  }
)

#' @describeIn normal_mix Normal mixture random generation
#' @export
rNormMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    K <- length(w)
    wsum <- 0.0
    for (j in 1:K) wsum <- wsum + w[j]
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
    return(rnorm(1, mean[idx], sd[idx]))
  }
)

#' @describeIn normal_mix Normal mixture quantile function
#' @export
qNormMix <- function(p, w, mean, sd,
                     lower.tail = TRUE, log.p = FALSE,
                     tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- -Inf; next }
    if (pi >= 1) { out[i] <- Inf; next }

    lo <- min(stats::qnorm(pi, mean = mean, sd = sd), na.rm = TRUE)
    hi <- max(stats::qnorm(pi, mean = mean, sd = sd), na.rm = TRUE)
    if (!is.finite(lo)) lo <- -1e20
    if (!is.finite(hi)) hi <- 1e20
    if (lo == hi) {
      out[i] <- lo
    } else {
      f_lo <- as.numeric(pNormMix(lo, w = w, mean = mean, sd = sd, lower.tail = 1, log.p = 0) - pi)
      f_hi <- as.numeric(pNormMix(hi, w = w, mean = mean, sd = sd, lower.tail = 1, log.p = 0) - pi)
      iter <- 0L
      while (is.finite(f_lo) && f_lo > 0 && lo > -1e20 && iter < 60L) {
        step <- max(1, abs(lo))
        lo <- lo - step
        f_lo <- as.numeric(pNormMix(lo, w = w, mean = mean, sd = sd, lower.tail = 1, log.p = 0) - pi)
        iter <- iter + 1L
      }
      iter <- 0L
      while (is.finite(f_hi) && f_hi < 0 && hi < 1e20 && iter < 60L) {
        step <- max(1, abs(hi))
        hi <- hi + step
        f_hi <- as.numeric(pNormMix(hi, w = w, mean = mean, sd = sd, lower.tail = 1, log.p = 0) - pi)
        iter <- iter + 1L
      }
      if (!is.finite(lo) || !is.finite(hi) || lo >= hi || !is.finite(f_lo) || !is.finite(f_hi) || f_lo * f_hi > 0) {
        out[i] <- NA_real_
      } else {
        out[i] <- stats::uniroot(
          function(z) pNormMix(z, w = w, mean = mean, sd = sd, lower.tail = 1, log.p = 0) - pi,
          interval = c(lo, hi),
          tol = tol, maxiter = maxiter
        )$root
      }
    }
  }
  out
}

# -------------------------------
# Normal mixture + GPD tail
# -------------------------------

#' Normal mixture with a Gpd tail
#'
#' Splices a generalized Pareto distribution (Gpd) above \code{threshold} onto a Normal mixture bulk.
#' The bulk probability at the threshold is used to scale the tail so that the overall CDF is proper.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param mean,sd Numeric vectors of length \eqn{K} giving component means and standard deviations.
#' @param threshold Numeric scalar threshold at which the Gpd tail is attached.
#' @param tail_scale Numeric scalar Gpd scale parameter; must be positive.
#' @param tail_shape Numeric scalar Gpd shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot}.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qNormMixGpd} returns a numeric vector
#'   with the same length as \code{p}.
#' @examples
#' w <- c(0.60, 0.25, 0.15)
#' mean <- c(-1, 0.5, 2.0)
#' sd <- c(1.0, 0.7, 1.3)
#' threshold <- 2
#' tail_scale <- 1.0
#' tail_shape <- 0.2
#'
#' dNormMixGpd(3.0, w, mean, sd, threshold, tail_scale, tail_shape, log = FALSE)
#' pNormMixGpd(3.0, w, mean, sd, threshold, tail_scale, tail_shape,
#'            lower.tail = TRUE, log.p = FALSE)
#' qNormMixGpd(0.50, w, mean, sd, threshold, tail_scale, tail_shape)
#' qNormMixGpd(0.95, w, mean, sd, threshold, tail_scale, tail_shape)
#' replicate(10, rNormMixGpd(1, w, mean, sd, threshold, tail_scale, tail_shape))
#' @rdname normal_mixgpd
#' @name normal_mixgpd
#' @aliases dNormMixGpd pNormMixGpd rNormMixGpd qNormMixGpd
#' @importFrom stats uniroot pnorm dnorm runif qnorm rnorm
NULL

#' @describeIn normal_mixgpd Normal mixture + Gpd tail density
#' @export
dNormMixGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (x < threshold) return(dNormMix(x, w, mean, sd, log))

    Fu <- pNormMix(threshold, w, mean, sd, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn normal_mixgpd Normal mixture + Gpd tail distribution function
#' @export
pNormMixGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (q < threshold) return(pNormMix(q, w, mean, sd, lower.tail, log.p))

    Fu <- pNormMix(threshold, w, mean, sd, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    G <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(max(cdf, eps))) else return(cdf)
  }
)

#' @describeIn normal_mixgpd Normal mixture + Gpd tail random generation
#' @export
rNormMixGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pNormMix(threshold, w, mean, sd, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rNormMix(1, w, mean, sd))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)

#' @describeIn normal_mixgpd Normal mixture + Gpd tail quantile function
#' @export
qNormMixGpd <- function(p, w, mean, sd, threshold, tail_scale, tail_shape,
                        lower.tail = TRUE, log.p = FALSE,
                        tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- pNormMix(threshold, w, mean, sd, 1, 0)
  out <- numeric(length(p))

  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- qNormMix(pi, w, mean, sd, lower.tail = TRUE, log.p = FALSE, tol = tol, maxiter = maxiter)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}

# -------------------------------
# Single Normal + GPD tail
# -------------------------------

#' Normal with a Gpd tail
#'
#' Splices a generalized Pareto distribution (Gpd) above \code{threshold} onto a single Normal bulk.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param mean Numeric scalar mean parameter for the Normal bulk.
#' @param sd Numeric scalar standard deviation for the Normal bulk.
#' @param threshold Numeric scalar threshold at which the Gpd tail is attached.
#' @param tail_scale Numeric scalar Gpd scale parameter; must be positive.
#' @param tail_shape Numeric scalar Gpd shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. \code{qNormGpd} returns a numeric vector
#'   with the same length as \code{p}.
#' @examples
#' mean <- 0.5
#' sd <- 1.0
#' threshold <- 2
#' tail_scale <- 1.0
#' tail_shape <- 0.2
#'
#' dNormGpd(3.0, mean, sd, threshold, tail_scale, tail_shape, log = FALSE)
#' pNormGpd(3.0, mean, sd, threshold, tail_scale, tail_shape,
#'         lower.tail = TRUE, log.p = FALSE)
#' qNormGpd(0.50, mean, sd, threshold, tail_scale, tail_shape)
#' qNormGpd(0.95, mean, sd, threshold, tail_scale, tail_shape)
#' replicate(10, rNormGpd(1, mean, sd, threshold, tail_scale, tail_shape))
#' @rdname normal_gpd
#' @name normal_gpd
#' @aliases dNormGpd pNormGpd rNormGpd qNormGpd
#' @importFrom stats uniroot pnorm dnorm runif qnorm rnorm
NULL

#' @describeIn normal_gpd Normal + Gpd tail density
#' @export
dNormGpd <- nimble::nimbleFunction(
  run = function(x = double(0),
                 mean = double(0),
                 sd = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (x < threshold) {
      dens <- dnorm(x, mean, sd, 0)
      if (dens < eps) dens <- eps
      if (log == 1) return(log(dens)) else return(dens)
    }

    Fu <- pnorm(threshold, mean, sd, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    val <- (1.0 - Fu) * dGpd(x, threshold, tail_scale, tail_shape, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn normal_gpd Normal + Gpd tail distribution function
#' @export
pNormGpd <- nimble::nimbleFunction(
  run = function(q = double(0),
                 mean = double(0),
                 sd = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (q < threshold) {
      cdf0 <- pnorm(q, mean, sd, lower.tail, log.p)
      return(cdf0)
    }

    Fu <- pnorm(threshold, mean, sd, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    G <- pGpd(q, threshold, tail_scale, tail_shape, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p == 1) return(log(max(cdf, eps))) else return(cdf)
  }
)

#' @describeIn normal_gpd Normal + Gpd tail random generation
#' @export
rNormGpd <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 mean = double(0),
                 sd = double(0),
                 threshold = double(0),
                 tail_scale = double(0),
                 tail_shape = double(0)) {
    returnType(double(0))

    if (n != 1) return(0.0)

    Fu <- pnorm(threshold, mean, sd, 1, 0)
    if (Fu < 0.0) Fu <- 0.0
    if (Fu > 1.0) Fu <- 1.0

    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rnorm(1, mean, sd))
    return(rGpd(1, threshold, tail_scale, tail_shape))
  }
)

#' @describeIn normal_gpd Normal + Gpd tail quantile function
#' @export
qNormGpd <- function(p, mean, sd, threshold, tail_scale, tail_shape,
                     lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  Fu <- stats::pnorm(threshold, mean = mean, sd = sd, lower.tail = TRUE, log.p = FALSE)
  out <- numeric(length(p))

  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) {
      out[i] <- stats::qnorm(pi, mean = mean, sd = sd, lower.tail = TRUE, log.p = FALSE)
    } else {
      g <- if (Fu >= 1) 0 else (pi - Fu) / (1 - Fu)
      out[i] <- qGpd(g, threshold = threshold, scale = tail_scale, shape = tail_shape)
    }
  }
  out
}


# ==========================================================
# Lowercase vectorized R wrappers for Normal kernels
# ==========================================================

#' Lowercase vectorized Normal distribution functions
#'
#' Vectorized R wrappers for Normal mixture, Normal mixture + GPD, and
#' Normal + GPD distribution functions. These lowercase versions accept vector
#' inputs for the first argument (\code{x}, \code{q}, or \code{p}) and return
#' a numeric vector. The \code{r*} functions support \code{n > 1}.
#'
#' @param x Numeric vector of quantiles.
#' @param q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Integer number of observations to generate.
#' @param w Numeric vector of mixture weights.
#' @param mean,sd Numeric vectors (mix) or scalars (base+gpd) of component parameters.
#' @param threshold,tail_scale,tail_shape GPD tail parameters (scalars).
#' @param log Logical; if \code{TRUE}, return log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le x)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are on log scale.
#' @param tol,maxiter Tolerance and max iterations for numerical inversion.
#'
#' @return Numeric vector of densities, probabilities, quantiles, or random variates.
#'
#' @examples
#' w <- c(0.6, 0.25, 0.15)
#' mu <- c(-1, 0.5, 2)
#' sig <- c(1, 0.7, 1.3)
#'
#' # Normal mixture
#' dnormmix(c(0, 1, 2), w = w, mean = mu, sd = sig)
#' rnormmix(5, w = w, mean = mu, sd = sig)
#'
#' # Normal mixture + GPD
#' dnormmixgpd(c(1, 2, 3), w = w, mean = mu, sd = sig,
#'             threshold = 2, tail_scale = 1, tail_shape = 0.2)
#'
#' # Normal + GPD (single component)
#' dnormgpd(c(1, 2, 3), mean = 0.5, sd = 1, threshold = 2,
#'          tail_scale = 1, tail_shape = 0.2)
#'
#' @name normal_lowercase
#' @rdname normal_lowercase
NULL

# ---- Normal Mix lowercase wrappers ----

#' @describeIn normal_lowercase Normal mixture density (vectorized)
#' @export
dnormmix <- function(x, w, mean, sd, log = FALSE) {
  x <- as.numeric(x)

  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dNormMix(xi, w = w, mean = mean, sd = sd, log = log_int)),
         numeric(1L))
}

#' @describeIn normal_lowercase Normal mixture distribution function (vectorized)
#' @export
pnormmix <- function(q, w, mean, sd, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pNormMix(qi, w = w, mean = mean, sd = sd,
                                              lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn normal_lowercase Normal mixture quantile function (vectorized)
#' @export
qnormmix <- function(p, w, mean, sd, lower.tail = TRUE, log.p = FALSE,
                     tol = 1e-10, maxiter = 200) {
  qNormMix(p, w = w, mean = mean, sd = sd, lower.tail = lower.tail,
           log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn normal_lowercase Normal mixture random generation (vectorized)
#' @export
rnormmix <- function(n, w, mean, sd) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rNormMix(1L, w = w, mean = mean, sd = sd)),
         numeric(1L))
}

# ---- Normal Mix + GPD lowercase wrappers ----

#' @describeIn normal_lowercase Normal mixture + GPD density (vectorized)
#' @export
dnormmixgpd <- function(x, w, mean, sd, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dNormMixGpd(xi, w = w, mean = mean, sd = sd,
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn normal_lowercase Normal mixture + GPD distribution function (vectorized)
#' @export
pnormmixgpd <- function(q, w, mean, sd, threshold, tail_scale, tail_shape,
                        lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pNormMixGpd(qi, w = w, mean = mean, sd = sd,
                                                 threshold = threshold, tail_scale = tail_scale,
                                                 tail_shape = tail_shape,
                                                 lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn normal_lowercase Normal mixture + GPD quantile function (vectorized)
#' @export
qnormmixgpd <- function(p, w, mean, sd, threshold, tail_scale, tail_shape,
                        lower.tail = TRUE, log.p = FALSE, tol = 1e-10, maxiter = 200) {
  qNormMixGpd(p, w = w, mean = mean, sd = sd, threshold = threshold,
              tail_scale = tail_scale, tail_shape = tail_shape,
              lower.tail = lower.tail, log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn normal_lowercase Normal mixture + GPD random generation (vectorized)
#' @export
rnormmixgpd <- function(n, w, mean, sd, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rNormMixGpd(1L, w = w, mean = mean, sd = sd,
                                                         threshold = threshold, tail_scale = tail_scale,
                                                         tail_shape = tail_shape)),
         numeric(1L))
}

# ---- Normal + GPD lowercase wrappers ----

#' @describeIn normal_lowercase Normal + GPD density (vectorized)
#' @export
dnormgpd <- function(x, mean, sd, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dNormGpd(xi, mean = mean, sd = sd,
                                              threshold = threshold, tail_scale = tail_scale,
                                              tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn normal_lowercase Normal + GPD distribution function (vectorized)
#' @export
pnormgpd <- function(q, mean, sd, threshold, tail_scale, tail_shape,
                     lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pNormGpd(qi, mean = mean, sd = sd,
                                              threshold = threshold, tail_scale = tail_scale,
                                              tail_shape = tail_shape,
                                              lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn normal_lowercase Normal + GPD quantile function (vectorized)
#' @export
qnormgpd <- function(p, mean, sd, threshold, tail_scale, tail_shape,
                     lower.tail = TRUE, log.p = FALSE) {
  qNormGpd(p, mean = mean, sd = sd, threshold = threshold,
           tail_scale = tail_scale, tail_shape = tail_shape,
           lower.tail = lower.tail, log.p = log.p)
}

#' @describeIn normal_lowercase Normal + GPD random generation (vectorized)
#' @export
rnormgpd <- function(n, mean, sd, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rNormGpd(1L, mean = mean, sd = sd,
                                                      threshold = threshold, tail_scale = tail_scale,
                                                      tail_shape = tail_shape)),
         numeric(1L))
}

