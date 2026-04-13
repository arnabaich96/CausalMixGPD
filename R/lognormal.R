#' Lognormal mixture distribution
#'
#' Finite mixture of lognormal components for positive-support bulk modeling. The scalar functions
#' in this topic are the NIMBLE-compatible building blocks for the lognormal bulk kernel family.
#'
#' The mixture density is
#' \deqn{
#' f(x) = \sum_{k = 1}^K \tilde{w}_k f_{LN}(x \mid \mu_k, \sigma_k),
#' \qquad x > 0,
#' }
#' with normalized weights \eqn{\tilde{w}_k}. For vectorized R usage, use [lognormal_lowercase()].
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
#' @return Density/CDF/RNG functions return numeric scalars. `qLognormalMix()` returns a numeric
#'   vector with the same length as `p`.
#'
#' @details
#' Each component satisfies \eqn{\log X_k \sim N(\mu_k,\sigma_k^2)}, so the mixture CDF is
#' \deqn{
#' F(x) = \sum_{k=1}^K \tilde{w}_k
#' \Phi\left(\frac{\log x-\mu_k}{\sigma_k}\right), \qquad x>0.
#' }
#' Random generation proceeds by drawing a component index with probability \eqn{\tilde{w}_k} and
#' then sampling from the corresponding lognormal law. Because a finite mixture of lognormals does
#' not admit a closed-form inverse CDF, \code{qLognormalMix()} computes quantiles by numerical
#' inversion.
#'
#' The analytical mixture mean is
#' \deqn{
#' E(X) = \sum_{k=1}^K \tilde{w}_k \exp(\mu_k + \sigma_k^2/2),
#' }
#' which is the expression used by the package whenever an ordinary predictive mean exists.
#'
#' @seealso [lognormal_mixgpd()], [lognormal_gpd()], [lognormal_lowercase()],
#'   [build_nimble_bundle()], [kernel_support_table()].
#' @family lognormal kernel families
#'
#' @examples
#' w <- c(0.60, 0.25, 0.15)
#' meanlog <- c(-0.2, 0.6, 1.2)
#' sdlog <- c(0.4, 0.3, 0.5)
#'
#' dLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog, log = FALSE)
#' pLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog,
#'              lower.tail = TRUE, log.p = FALSE)
#' qLognormalMix(0.50, w = w, meanlog = meanlog, sdlog = sdlog)
#' qLognormalMix(0.95, w = w, meanlog = meanlog, sdlog = sdlog)
#' replicate(10, rLognormalMix(1, w = w, meanlog = meanlog, sdlog = sdlog))
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

    hi <- max(stats::qlnorm(pi, meanlog = meanlog, sdlog = sdlog), na.rm = TRUE)
    if (!is.finite(hi) || hi <= 0) hi <- 1
    f0 <- as.numeric(pLognormalMix(0, w = w, meanlog = meanlog, sdlog = sdlog, 1, 0) - pi)
    fhi <- as.numeric(pLognormalMix(hi, w = w, meanlog = meanlog, sdlog = sdlog, 1, 0) - pi)
    iter <- 0L
    while (is.finite(fhi) && f0 * fhi > 0 && hi < 1e20 && iter < 60L) {
      hi <- hi * 2
      fhi <- as.numeric(pLognormalMix(hi, w = w, meanlog = meanlog, sdlog = sdlog, 1, 0) - pi)
      iter <- iter + 1L
    }
    if (!is.finite(hi) || hi <= 0 || !is.finite(fhi) || f0 * fhi > 0) {
      out[i] <- Inf
    } else {
      out[i] <- stats::uniroot(
        function(z) as.numeric(pLognormalMix(z, w = w, meanlog = meanlog, sdlog = sdlog, 1, 0)) - pi,
        interval = c(0, hi),
        tol = tol, maxiter = maxiter
      )$root
    }
  }
  out
}

meanLognormalMix <- function(w, meanlog, sdlog) {
  ww <- .mean_norm_mix_weights(w)
  if (is.null(ww)) return(NA_real_)
  mu <- as.numeric(meanlog)
  sig <- as.numeric(sdlog)
  if (length(mu) != length(ww) || length(sig) != length(ww)) return(NA_real_)
  if (any(!is.finite(mu)) || any(!is.finite(sig)) || any(sig <= 0)) return(NA_real_)
  sum(ww * exp(mu + 0.5 * sig^2))
}

meanLognormalMixTrunc <- function(w, meanlog, sdlog, threshold) {
  ww <- .mean_norm_mix_weights(w)
  if (is.null(ww)) return(NA_real_)
  mu <- as.numeric(meanlog)
  sig <- as.numeric(sdlog)
  u <- as.numeric(threshold)[1]
  if (length(mu) != length(ww) || length(sig) != length(ww)) return(NA_real_)
  if (any(!is.finite(mu)) || any(!is.finite(sig)) || any(sig <= 0) || is.na(u)) return(NA_real_)
  if (u <= 0) return(0)
  z <- (log(u) - mu - sig^2) / sig
  sum(ww * exp(mu + 0.5 * sig^2) * stats::pnorm(z))
}

#' Lognormal mixture with a GPD tail
#'
#' Spliced bulk-tail family formed by attaching a generalized Pareto tail to a lognormal mixture
#' bulk.
#' Let \eqn{F_{mix}} be the Lognormal mixture CDF. The spliced CDF is
#' \eqn{F(x)=F_{mix}(x)} for \eqn{x<threshold} and
#' \eqn{F(x)=F_{mix}(threshold) + \{1-F_{mix}(threshold)\}G(x)} for \eqn{x\ge threshold}, where \eqn{G}
#' is the GPD CDF for exceedances above \code{threshold}.
#'
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s. The quantile is an R function:
#' it uses numerical inversion in the bulk region and the closed-form GPD quantile in the tail.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param meanlog,sdlog Numeric vectors of length \eqn{K} giving component log-means and log-standard deviations.
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars.
#'   `qLognormalMixGpd()` returns a numeric vector with the same length as `p`.
#'
#' @details
#' Let \eqn{F_{mix}} be the lognormal-mixture CDF and let \eqn{u} denote the threshold. The splice
#' uses the bulk law below \eqn{u} and attaches a GPD to the residual survival mass above
#' \eqn{u}. The density therefore becomes
#' \deqn{
#' f(x) =
#' \left\{
#' \begin{array}{ll}
#' f_{mix}(x), & x < u, \\
#' \{1-F_{mix}(u)\} g_{GPD}(x \mid u,\sigma_u,\xi), & x \ge u.
#' \end{array}
#' \right.
#' }
#' The quantile is computed piecewise: bulk quantiles are obtained numerically from the mixture CDF,
#' whereas tail quantiles use the closed-form GPD inverse after rescaling the upper-tail
#' probability.
#'
#' @seealso [lognormal_mix()], [lognormal_gpd()], [gpd()], [lognormal_lowercase()], [dpmgpd()].
#' @family lognormal kernel families
#'
#' @examples
#' w <- c(0.60, 0.25, 0.15)
#' meanlog <- c(-0.2, 0.6, 1.2)
#' sdlog <- c(0.4, 0.3, 0.5)
#' threshold <- 3
#' tail_scale <- 0.9
#' tail_shape <- 0.2
#'
#' dLognormalMixGpd(4.0, w = w, meanlog = meanlog, sdlog = sdlog,
#'                 threshold = threshold, tail_scale = tail_scale,
#'                 tail_shape = tail_shape, log = FALSE)
#' pLognormalMixGpd(4.0, w = w, meanlog = meanlog, sdlog = sdlog,
#'                 threshold = threshold, tail_scale = tail_scale,
#'                 tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE)
#' qLognormalMixGpd(0.50, w = w, meanlog = meanlog, sdlog = sdlog,
#'                 threshold = threshold, tail_scale = tail_scale,
#'                 tail_shape = tail_shape)
#' qLognormalMixGpd(0.95, w = w, meanlog = meanlog, sdlog = sdlog,
#'                 threshold = threshold, tail_scale = tail_scale,
#'                 tail_shape = tail_shape)
#' replicate(10, rLognormalMixGpd(1, w = w, meanlog = meanlog, sdlog = sdlog,
#'                               threshold = threshold,
#'                               tail_scale = tail_scale,
#'                               tail_shape = tail_shape))
#' @rdname lognormal_mixgpd
#' @name lognormal_mixgpd
#' @aliases dLognormalMixGpd pLognormalMixGpd rLognormalMixGpd qLognormalMixGpd
NULL

#' @describeIn lognormal_mixgpd Lognormal mixture + GPD tail density
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

#' @describeIn lognormal_mixgpd Lognormal mixture + GPD tail distribution function
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

#' @describeIn lognormal_mixgpd Lognormal mixture + GPD tail random generation
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

#' @describeIn lognormal_mixgpd Lognormal mixture + GPD tail quantile function
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




#' Lognormal with a GPD tail
#'
#' Spliced family obtained by attaching a generalized Pareto tail above `threshold` to a single
#' lognormal bulk.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. The RNG implementation supports \code{n = 1}.
#' @param meanlog Numeric scalar log-mean parameter for the Lognormal bulk.
#' @param sdlog Numeric scalar log-standard deviation for the Lognormal bulk.
#' @param threshold Numeric scalar threshold at which the GPD tail is attached.
#' @param tail_scale Numeric scalar GPD scale parameter; must be positive.
#' @param tail_shape Numeric scalar GPD shape parameter.
#' @param log Integer flag \code{0/1}; if \code{1}, return the log-density.
#' @param lower.tail Integer flag \code{0/1}; if \code{1} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Integer flag \code{0/1}; if \code{1}, probabilities are returned on the log scale.
#'
#' @return Spliced density/CDF/RNG functions return numeric scalars. `qLognormalGpd()` returns a
#'   numeric vector with the same length as `p`.
#'
#' @details
#' This is the single-lognormal counterpart of [lognormal_mixgpd()]. If \eqn{F_{LN}(u)} denotes the
#' bulk probability below the threshold, then the spliced density is
#' \deqn{
#' f(x) =
#' \left\{
#' \begin{array}{ll}
#' f_{LN}(x \mid \mu,\sigma), & x < u, \\
#' \{1-F_{LN}(u)\} g_{GPD}(x \mid u,\sigma_u,\xi), & x \ge u.
#' \end{array}
#' \right.
#' }
#' The ordinary mean is finite only when the GPD tail has \eqn{\xi < 1};
#' otherwise the package requires restricted means or quantiles.
#'
#' @seealso [lognormal_mix()], [lognormal_mixgpd()], [gpd()], [lognormal_lowercase()].
#' @family lognormal kernel families
#'
#' @examples
#' meanlog <- 0.4
#' sdlog <- 0.35
#' threshold <- 3
#' tail_scale <- 0.9
#' tail_shape <- 0.2
#'
#' dLognormalGpd(4.0, meanlog, sdlog, threshold, tail_scale, tail_shape, log = FALSE)
#' pLognormalGpd(4.0, meanlog, sdlog, threshold, tail_scale, tail_shape,
#'              lower.tail = TRUE, log.p = FALSE)
#' qLognormalGpd(0.50, meanlog, sdlog, threshold, tail_scale, tail_shape)
#' qLognormalGpd(0.95, meanlog, sdlog, threshold, tail_scale, tail_shape)
#' replicate(10, rLognormalGpd(1, meanlog, sdlog, threshold, tail_scale, tail_shape))
#' @rdname lognormal_gpd
#' @name lognormal_gpd
#' @aliases dLognormalGpd pLognormalGpd rLognormalGpd qLognormalGpd
NULL

#' @describeIn lognormal_gpd Lognormal + GPD tail density
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

#' @describeIn lognormal_gpd Lognormal + GPD tail distribution function
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

#' @describeIn lognormal_gpd Lognormal + GPD tail random generation
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

#' @describeIn lognormal_gpd Lognormal + GPD tail quantile function
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


# ==========================================================
# Lowercase vectorized R wrappers for Lognormal kernels
# ==========================================================

#' Lowercase vectorized lognormal distribution functions
#'
#' Vectorized R wrappers for the scalar lognormal-kernel topics in this file.
#'
#' @param x Numeric vector of quantiles.
#' @param q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Integer number of observations to generate.
#' @param w Numeric vector of mixture weights.
#' @param meanlog,sdlog Numeric vectors (mix) or scalars (base+gpd) of component parameters.
#' @param threshold,tail_scale,tail_shape GPD tail parameters (scalars).
#' @param log Logical; if \code{TRUE}, return log-density.
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le x)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are on log scale.
#' @param tol,maxiter Tolerance and max iterations for numerical inversion.
#'
#' @return Numeric vector of densities, probabilities, quantiles, or random variates.
#'
#' @details
#' These are direct vectorized wrappers around the scalar lognormal routines. They keep the same
#' parameterization, support restrictions, and bulk-tail splice, while allowing ordinary vector
#' inputs in R. Quantile wrappers continue to use the scalar inversion logic, so there is no
#' separate approximation layer in the lowercase API.
#'
#' @seealso [lognormal_mix()], [lognormal_mixgpd()], [lognormal_gpd()], [bundle()],
#'   [get_kernel_registry()].
#' @family vectorized kernel helpers
#'
#' @examples
#' w <- c(0.6, 0.3, 0.1)
#' ml <- c(0, 0.3, 0.6)
#' sl <- c(0.4, 0.5, 0.6)
#'
#' # Lognormal mixture
#' dlognormalmix(c(1, 2, 3), w = w, meanlog = ml, sdlog = sl)
#' rlognormalmix(5, w = w, meanlog = ml, sdlog = sl)
#'
#' # Lognormal mixture + GPD
#' dlognormalmixgpd(c(2, 3, 4), w = w, meanlog = ml, sdlog = sl,
#'                  threshold = 2.5, tail_scale = 0.5, tail_shape = 0.2)
#'
#' @name lognormal_lowercase
#' @rdname lognormal_lowercase
NULL

# ---- Lognormal Mix lowercase wrappers ----

#' @describeIn lognormal_lowercase Lognormal mixture density (vectorized)
#' @export
dlognormalmix <- function(x, w, meanlog, sdlog, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dLognormalMix(xi, w = w, meanlog = meanlog, sdlog = sdlog, log = log_int)),
         numeric(1L))
}

#' @describeIn lognormal_lowercase Lognormal mixture distribution function (vectorized)
#' @export
plognormalmix <- function(q, w, meanlog, sdlog, lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pLognormalMix(qi, w = w, meanlog = meanlog, sdlog = sdlog,
                                                   lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn lognormal_lowercase Lognormal mixture quantile function (vectorized)
#' @export
qlognormalmix <- function(p, w, meanlog, sdlog, lower.tail = TRUE, log.p = FALSE,
                          tol = 1e-10, maxiter = 200) {
  qLognormalMix(p, w = w, meanlog = meanlog, sdlog = sdlog, lower.tail = lower.tail,
                log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn lognormal_lowercase Lognormal mixture random generation (vectorized)
#' @export
rlognormalmix <- function(n, w, meanlog, sdlog) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rLognormalMix(1L, w = w, meanlog = meanlog, sdlog = sdlog)),
         numeric(1L))
}

# ---- Lognormal Mix + GPD lowercase wrappers ----

#' @describeIn lognormal_lowercase Lognormal mixture + GPD density (vectorized)
#' @export
dlognormalmixgpd <- function(x, w, meanlog, sdlog, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dLognormalMixGpd(xi, w = w, meanlog = meanlog, sdlog = sdlog,
                                                      threshold = threshold, tail_scale = tail_scale,
                                                      tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn lognormal_lowercase Lognormal mixture + GPD distribution function (vectorized)
#' @export
plognormalmixgpd <- function(q, w, meanlog, sdlog, threshold, tail_scale, tail_shape,
                             lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pLognormalMixGpd(qi, w = w, meanlog = meanlog, sdlog = sdlog,
                                                      threshold = threshold, tail_scale = tail_scale,
                                                      tail_shape = tail_shape,
                                                      lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn lognormal_lowercase Lognormal mixture + GPD quantile function (vectorized)
#' @export
qlognormalmixgpd <- function(p, w, meanlog, sdlog, threshold, tail_scale, tail_shape,
                             lower.tail = TRUE, log.p = FALSE, tol = 1e-10, maxiter = 200) {
  qLognormalMixGpd(p, w = w, meanlog = meanlog, sdlog = sdlog, threshold = threshold,
                   tail_scale = tail_scale, tail_shape = tail_shape,
                   lower.tail = lower.tail, log.p = log.p, tol = tol, maxiter = maxiter)
}

#' @describeIn lognormal_lowercase Lognormal mixture + GPD random generation (vectorized)
#' @export
rlognormalmixgpd <- function(n, w, meanlog, sdlog, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rLognormalMixGpd(1L, w = w, meanlog = meanlog, sdlog = sdlog,
                                                              threshold = threshold, tail_scale = tail_scale,
                                                              tail_shape = tail_shape)),
         numeric(1L))
}

# ---- Lognormal + GPD lowercase wrappers ----

#' @describeIn lognormal_lowercase Lognormal + GPD density (vectorized)
#' @export
dlognormalgpd <- function(x, meanlog, sdlog, threshold, tail_scale, tail_shape, log = FALSE) {
  x <- as.numeric(x)
  if (length(x) == 0L) return(numeric(0L))
  log_int <- as.integer(log)
  vapply(x, function(xi) as.numeric(dLognormalGpd(xi, meanlog = meanlog, sdlog = sdlog,
                                                   threshold = threshold, tail_scale = tail_scale,
                                                   tail_shape = tail_shape, log = log_int)),
         numeric(1L))
}

#' @describeIn lognormal_lowercase Lognormal + GPD distribution function (vectorized)
#' @export
plognormalgpd <- function(q, meanlog, sdlog, threshold, tail_scale, tail_shape,
                          lower.tail = TRUE, log.p = FALSE) {
  q <- as.numeric(q)
  if (length(q) == 0L) return(numeric(0L))
  lt_int <- as.integer(lower.tail)
  lp_int <- as.integer(log.p)
  vapply(q, function(qi) as.numeric(pLognormalGpd(qi, meanlog = meanlog, sdlog = sdlog,
                                                   threshold = threshold, tail_scale = tail_scale,
                                                   tail_shape = tail_shape,
                                                   lower.tail = lt_int, log.p = lp_int)),
         numeric(1L))
}

#' @describeIn lognormal_lowercase Lognormal + GPD quantile function (vectorized)
#' @export
qlognormalgpd <- function(p, meanlog, sdlog, threshold, tail_scale, tail_shape,
                          lower.tail = TRUE, log.p = FALSE) {
  qLognormalGpd(p, meanlog = meanlog, sdlog = sdlog, threshold = threshold,
                tail_scale = tail_scale, tail_shape = tail_shape,
                lower.tail = lower.tail, log.p = log.p)
}

#' @describeIn lognormal_lowercase Lognormal + GPD random generation (vectorized)
#' @export
rlognormalgpd <- function(n, meanlog, sdlog, threshold, tail_scale, tail_shape) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n)) stop("'n' must be a single integer.", call. = FALSE)
  if (n <= 0L) return(numeric(0L))
  vapply(seq_len(n), function(i) as.numeric(rLognormalGpd(1L, meanlog = meanlog, sdlog = sdlog,
                                                          threshold = threshold, tail_scale = tail_scale,
                                                          tail_shape = tail_shape)),
         numeric(1L))
}

