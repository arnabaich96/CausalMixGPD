#' Normal mixture distribution
#'
#' This topic documents the finite-mixture version of the distribution.
#' The density, CDF, and RNG are implemented as \code{nimbleFunction}s so they can be used
#' inside NIMBLE models. The quantile function is an R function computed by numerical inversion
#' of the mixture CDF.
#'
#' Each component \eqn{j} is a Normal distribution with mean \code{mean[j]} and standard deviation \code{sd[j]}.
#' The argument \code{var} can be provided instead of \code{sd} (interpreted as \code{sd = sqrt(var)}).
#' The generic aliases \code{param1} and \code{param2} are accepted for \code{mean} and \code{sd}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE, RNG supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}. The functions normalize \code{w} internally when needed.
#' @param mean,sd Numeric vectors of length \eqn{K} giving component means and standard deviations.
#' @param var Numeric vector of length \eqn{K} giving component variances (optional alternative to \code{sd}).
#' @param param1,param2 Generic aliases for \code{mean} and \code{sd}.
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
#' w <- c(0.6, 0.4)
#' mu <- c(0, 3)
#' s <- c(1, 0.8)
#' dM <- compileNimble(dNormMix)
#' dM(0.5, w = w, mean = mu, sd = s)
#' qNormMix(0.95, w = w, mean = mu, sd = s)
#' }
#'
#' @rdname normal_mix
#' @name normal_mix
#' @aliases dNormMix pNormMix rNormMix qNormMix
NULL


#' Normal mixture with a GPD tail
#'
#' This topic documents the mixture distribution spliced with a generalized Pareto (GPD) tail above
#' a threshold \code{u}. The bulk mixture governs \eqn{x<u}. For \eqn{x\ge u}, exceedances follow a GPD
#' and are weighted by \eqn{1 - F_{mix}(u)} so that the overall distribution remains proper.
#'
#' The bulk distribution is a Normal mixture. A generalized Pareto tail is attached above \code{u}.
#'
#' @param x Numeric scalar giving the point at which the density is evaluated.
#' @param q Numeric scalar giving the point at which the distribution function is evaluated.
#' @param p Numeric scalar probability in \eqn{(0,1)} for the quantile function.
#' @param n Integer giving the number of draws. For portability inside NIMBLE, RNG supports \code{n = 1}.
#' @param w Numeric vector of mixture weights of length \eqn{K}.
#' @param mean,sd Numeric vectors of length \eqn{K} giving component means and standard deviations.
#' @param var Numeric vector of length \eqn{K} giving component variances (optional alternative to \code{sd}).
#' @param param1,param2 Generic aliases for \code{mean} and \code{sd}.
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
#' w <- c(0.6, 0.4)
#' mu <- c(0, 3)
#' s <- c(1, 0.8)
#' u <- 2
#' dSp <- compileNimble(dNormMixGPD)
#' dSp(3.0, w = w, mean = mu, sd = s, u = u, sigma = 1, xi = 0.2)
#' qNormMixGPD(0.99, w = w, mean = mu, sd = s, u = u, sigma = 1, xi = 0.2)
#' }
#'
#' @rdname normal_mixgpd
#' @name normal_mixgpd
#' @aliases dNormMixGPD pNormMixGPD rNormMixGPD qNormMixGPD
NULL


#' @describeIn normal_mix Normal mixture density
#' @export
dNormMix <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 var = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(mean) == 0) mean <- param1
    if (length(sd) == 0) sd <- param2
    if (length(sd) == 0 & length(var) > 0) sd <- sqrt(var)
    K <- length(w)
    s0 <- 0.0
    for (j in 1:K) s0 <- s0 + w[j] * dnorm(x, mean = mean[j], sd = sd[j], log = 0)
    if (s0 < eps) s0 <- eps
    if (log == 1) return(log(s0)) else return(s0)
  }
)

#' @describeIn normal_mix Normal mixture Distribution function
#' @export
pNormMix <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 var = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (length(mean) == 0) mean <- param1
    if (length(sd) == 0) sd <- param2
    if (length(sd) == 0 & length(var) > 0) sd <- sqrt(var)
    K <- length(w)
    cdf <- 0.0
    for (j in 1:K) cdf <- cdf + w[j] * pnorm(q, mean = mean[j], sd = sd[j], lower.tail = 1, log.p = 0)
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn normal_mix Normal mixture random generation
#' @export
rNormMix <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 var = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1)) {
    returnType(double(0))
    if (length(mean) == 0) mean <- param1
    if (length(sd) == 0) sd <- param2
    if (length(sd) == 0 & length(var) > 0) sd <- sqrt(var)
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
    return(rnorm(1, mean = mean[idx], sd = sd[idx]))
  }
)

#' @describeIn normal_mix Normal mixture quantile function
#' @export
qNormMix <- function(p, w, mean, sd, var = NULL, param1 = mean, param2 = sd,
                     lower.tail = TRUE, log.p = FALSE,
                     tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(mean) == 0) mean <- as.numeric(param1)
  if (length(sd) == 0) sd <- as.numeric(param2)
  if ((length(sd) == 0) & !is.null(var)) sd <- sqrt(as.numeric(var))

  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= 0) { out[i] <- -Inf; next }
    if (pi >= 1) { out[i] <- Inf; next }
    out[i] <- stats::uniroot(function(z) pNormMix(z,w,mean,sd) - pi, interval = c(-1e10,1e10), tol = tol, maxiter = maxiter)$root
  }
  out
}

#' @describeIn normal_mixgpd Normal mixture + GPD tail density function
#' @export
dNormMixGPD <- nimble::nimbleFunction(
  run = function(x = double(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 var = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (x < u) return(dNormMix(x, w, mean, sd, var, param1, param2, log))
    Fu <- pNormMix(u, w, mean, sd, var, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    val <- (1.0 - Fu) * dGpd(x, u, sigma, xi, 0)
    if (val < eps) val <- eps
    if (log == 1) return(log(val)) else return(val)
  }
)

#' @describeIn normal_mixgpd Normal mixture + GPD tail Distribution function
#' @export
pNormMixGPD <- nimble::nimbleFunction(
  run = function(q = double(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 var = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0),
                 lower.tail = integer(0, default = 1),
                 log.p = integer(0, default = 0)) {
    returnType(double(0))
    eps <- 1e-300
    if (q < u) return(pNormMix(q, w, mean, sd, var, param1, param2, lower.tail, log.p))
    Fu <- pNormMix(u, w, mean, sd, var, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    G <- pGpd(q, u, sigma, xi, 1, 0)
    cdf <- Fu + (1.0 - Fu) * G
    cdf <- max(min(cdf, 1.0), 0.0)
    if (lower.tail == 0) cdf <- 1.0 - cdf
    if (log.p != 0) cdf <- log(max(cdf, eps))
    return(cdf)
  }
)

#' @describeIn normal_mixgpd Normal mixture + Gpd Tail random generation
#' @export
rNormMixGPD <- nimble::nimbleFunction(
  run = function(n = integer(0),
                 w = double(1),
                 mean = double(1),
                 sd = double(1),
                 var = double(1, default = numeric(0)),
                 param1 = double(1),
                 param2 = double(1),
                 u = double(0),
                 sigma = double(0),
                 xi = double(0)) {
    returnType(double(0))
    if (n != 1) return(0.0)
    Fu <- pNormMix(u, w, mean, sd, var, param1, param2, 1, 0)
    Fu <- max(min(Fu, 1.0), 0.0)
    uu <- runif(1, 0.0, 1.0)
    if (uu < Fu) return(rNormMix(1, w, mean, sd, var, param1, param2))
    return(rGpd(1, u, sigma, xi))
  }
)

#' @describeIn normal_mixgpd Normal mixture + GPD tail quantile function
#' @export
qNormMixGPD <- function(p, w, mean, sd, u, sigma, xi,
                        var = NULL, param1 = mean, param2 = sd,
                        lower.tail = TRUE, log.p = FALSE,
                        tol = 1e-10, maxiter = 200) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  p <- pmax(pmin(p, 1), 0)
  w <- as.numeric(w); w <- w / sum(w)
  if (length(mean) == 0) mean <- as.numeric(param1)
  if (length(sd) == 0) sd <- as.numeric(param2)
  if ((length(sd) == 0) & !is.null(var)) sd <- sqrt(as.numeric(var))
  Fu <- sum(w * stats::pnorm(u, mean = mean, sd = sd))
  Fu <- max(min(Fu, 1.0), 0.0)
  out <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[i]
    if (pi <= Fu) out[i] <- qNormMix(pi, w, mean, sd, lower.tail = TRUE, log.p = FALSE, tol = tol, maxiter = maxiter)
    else {
      g <- if (Fu >= 1) 0 else (pi - Fu)/(1 - Fu)
      out[i] <- qGpd(g, u = u, sigma = sigma, xi = xi)
    }
  }
  out
}




