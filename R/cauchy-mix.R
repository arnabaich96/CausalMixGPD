#' Cauchy mixture distribution
#'
#' A Cauchy mixture is a heavy-tailed, symmetric finite mixture that can be useful as a robust
#' bulk model when extreme observations are not well described by a single scale. This file
#' provides a NIMBLE-compatible density and CDF, a one-draw RNG, and an R-level quantile function
#' computed by numerical inversion.
#'
#' Each component \eqn{j} is a Cauchy distribution with location \code{location[j]} and
#' scale \code{scale[j]}. The arguments \code{loc} and \code{param1} are accepted as aliases for
#' \code{location}, and \code{param2} is accepted as an alias for \code{scale}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE,
#'   the RNG implementation supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w}
#'   internally when needed.
#' @param location Numeric vector of length \eqn{K} giving component locations.
#' @param scale Numeric vector of length \eqn{K} giving component scales (must be positive for a proper density).
#' @param loc Alias for \code{location}.
#' @param param1,param2 Generic aliases for \code{location} and \code{scale}.
#' @param log Logical; if \code{TRUE}, return the log-density (integer flag \code{0/1} in NIMBLE).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities are \eqn{P(X \le q)}.
#' @param log.p Logical; if \code{TRUE}, probabilities are returned on the log scale.
#' @param tol Numeric scalar tolerance passed to \code{stats::uniroot} in quantile inversion.
#' @param maxiter Integer maximum number of iterations for \code{stats::uniroot}.
#'
#' @return Density/CDF/RNG functions return numeric scalars. \code{qCauchyMix} returns a numeric
#'   vector with the same length as \code{p}.
#'
#' @examples
#' \dontrun{
#' library(nimble)
#' w <- c(0.7, 0.3)
#' loc <- c(0, 2)
#' sc  <- c(1, 0.5)
#'
#' dC <- compileNimble(dCauchyMix)
#' dC(0.25, w = w, location = loc, scale = sc)
#'
#' qCauchyMix(0.9, w = w, location = loc, scale = sc)
#' }
#'
#' @rdname cauchy_mix
#' @name cauchy_mix
#' @aliases dCauchyMix pCauchyMix rCauchyMix qCauchyMix
NULL



#' @describeIn cauchy_mix Density of Cauchy mixture.
#' @export
dCauchyMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 loc = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (length(location) == 0) location <- loc
    if (length(location) == 0) location <- param1
    if (length(scale) == 0) scale <- param2

    K <- length(w)
    s <- 0.0

    for (j in 1:K) {
      z <- (x - location[j]) / scale[j]
      # Cauchy(loc, scale) = t_df=1 on standardized scale, then /scale
      s <- s + w[j] * (dt(z, df = 1.0, log = 0) / scale[j])
    }

    if (s < eps) s <- eps
    if (log == 1) return(log(s)) else return(s)
  }
)


#' @describeIn cauchy_mix Distribution Function of Cauchy mixture.
#' @export
pCauchyMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 loc = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300

    if (length(location) == 0) location <- loc
    if (length(location) == 0) location <- param1
    if (length(scale) == 0) scale <- param2

    K <- length(w)
    cdf <- 0.0

    for (j in 1:K) {
      z <- (q - location[j]) / scale[j]
      cdf <- cdf + w[j] * pt(z, df = 1.0, lower.tail = 1, log.p = 0)
    }

    if (cdf < 0.0) cdf <- 0.0
    if (cdf > 1.0) cdf <- 1.0
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn cauchy_mix Random Generation of Cauchy mixture.
#' @export
rCauchyMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 location = double(1),
                 scale = double(1),
                 loc = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1)) {
    returnType(double(0))

    if (length(location) == 0) location <- loc
    if (length(location) == 0) location <- param1
    if (length(scale) == 0) scale <- param2

    if (n != 1) return(0.0)

    K <- length(w)
    u <- runif(1, 0.0, 1.0)

    # pick component without 'break'
    cw <- 0.0
    idx <- 1
    for (j in 1:K) {
      cw <- cw + w[j]
      if (u <= cw & idx == 1) idx <- j
    }

    # sample standard Cauchy via t(df=1), then shift/scale
    z <- rt(1, df = 1.0)
    return(location[idx] + scale[idx] * z)
  }
)

#' @describeIn cauchy_mix Quantile Function of Cauchy mixture.
#' @export
qCauchyMix <- function(p, w, location, scale,
                       loc = location, param1 = location, param2 = scale,
                       lower.tail = TRUE, log.p = FALSE,
                       tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)

  w <- as.numeric(w); w <- w / sum(w)
  if (length(location) == 0) location <- as.numeric(loc)
  if (length(location) == 0) location <- as.numeric(param1)
  if (length(scale) == 0) scale <- as.numeric(param2)

  if (length(w) != length(location) || length(w) != length(scale)) {
    stop("Lengths of w, location, and scale must match.", call. = FALSE)
  }

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- -Inf; next }
    if (pi >= 1) { out[i] <- Inf; next }
    out[i] <- stats::uniroot(function(z) pCauchyMix(q = z,
                                                    w = w,
                                                    location = location,
                                                    scale = scale,
                                                    loc = location,
                                                    param1 = location,
                                                    param2 = scale,) - pi,
                             interval = c(-1e10, 1e10), tol = tol, maxiter = maxiter)$root
  }

  out
}

