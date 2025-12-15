# Kernel: Inverse Gaussian ----------------------------------------------------
# Parametrization: mean (mu>0), shape (lambda>0)
# Density: sqrt(lambda/(2*pi*x^3)) * exp(-lambda*(x-mu)^2/(2*mu^2*x))
# CDF uses the normal CDF representation.
# RNG uses the Michael-Schucany-Haas method.

.kernel_invgauss_check <- function(mean, shape) {
  if (any(!is.finite(mean)) || any(mean <= 0)) stop("InvGauss: mean must be > 0.", call. = FALSE)
  if (any(!is.finite(shape)) || any(shape <= 0)) stop("InvGauss: shape must be > 0.", call. = FALSE)
}

.dinvgauss_internal <- function(x, mu, lambda, log = FALSE) {
  .kernel_invgauss_check(mu, lambda)
  if (any(x <= 0, na.rm = TRUE)) {
    out <- rep(if (log) -Inf else 0, length(x))
    ok <- x > 0
    if (!any(ok)) return(out)
    x2 <- x[ok]
    logdens <- 0.5 * (log(lambda) - log(2 * pi) - 3 * log(x2)) -
      (lambda * (x2 - mu)^2) / (2 * mu^2 * x2)
    out[ok] <- if (log) logdens else exp(logdens)
    return(out)
  }
  logdens <- 0.5 * (log(lambda) - log(2 * pi) - 3 * log(x)) -
    (lambda * (x - mu)^2) / (2 * mu^2 * x)
  if (log) logdens else exp(logdens)
}

.pinvgauss_internal <- function(q, mu, lambda, lower.tail = TRUE, log.p = FALSE) {
  .kernel_invgauss_check(mu, lambda)
  q <- as.numeric(q)
  cdf <- numeric(length(q))
  cdf[q <= 0] <- 0
  ok <- q > 0
  if (any(ok)) {
    x <- q[ok]
    z1 <- sqrt(lambda / x) * (x / mu - 1)
    z2 <- -sqrt(lambda / x) * (x / mu + 1)
    cdf[ok] <- stats::pnorm(z1) + exp(2 * lambda / mu) * stats::pnorm(z2)
    # Numerical guard
    cdf[ok] <- pmin(pmax(cdf[ok], 0), 1)
  }
  if (!lower.tail) cdf <- 1 - cdf
  if (log.p) return(log(cdf))
  cdf
}

.qinvgauss_internal <- function(p, mu, lambda, lower.tail = TRUE, log.p = FALSE,
                               tol = .Machine$double.eps^0.5, maxiter = 100) {
  .kernel_invgauss_check(mu, lambda)
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  if (any(p <= 0 | p >= 1, na.rm = TRUE)) stop("InvGauss: p must be in (0,1).", call. = FALSE)

  # crude bracket: start around mu, expand multiplicatively
  qfun <- function(x) .pinvgauss_internal(x, mu = mu, lambda = lambda) - p

  lower <- 0
  upper <- mu
  # ensure upper is above p
  for (k in seq_len(60)) {
    if (qfun(upper) >= 0) break
    upper <- upper * 2
  }
  if (qfun(upper) < 0) stop("InvGauss: failed to bracket quantile.", call. = FALSE)

  stats::uniroot(qfun, interval = c(lower, upper), tol = tol, maxiter = maxiter)$root
}

.rinvgauss_internal <- function(n, mu, lambda) {
  .kernel_invgauss_check(mu, lambda)
  v <- stats::rnorm(n)^2
  y <- mu + (mu^2 * v) / (2 * lambda) - (mu / (2 * lambda)) * sqrt(4 * mu * lambda * v + mu^2 * v^2)
  u <- stats::runif(n)
  x <- ifelse(u <= mu / (mu + y), y, (mu^2) / y)
  x
}

kernel_inverse_gaussian_def <- list(
  name   = "inverse_gaussian",
  params = c("mean", "shape"),
  support = c(lower = 0, upper = Inf),
  default_trans = list(mean = "exp", shape = NULL),

  d = function(y, mean, shape, log = FALSE) {
    .dinvgauss_internal(y, mu = mean, lambda = shape, log = log)
  },

  p = function(q, mean, shape, lower.tail = TRUE, log.p = FALSE) {
    .pinvgauss_internal(q, mu = mean, lambda = shape, lower.tail = lower.tail, log.p = log.p)
  },

  q = function(p, mean, shape, lower.tail = TRUE, log.p = FALSE) {
    .qinvgauss_internal(p, mu = mean, lambda = shape, lower.tail = lower.tail, log.p = log.p)
  },

  r = function(n, mean, shape) {
    .rinvgauss_internal(n, mu = mean, lambda = shape)
  }
)

if (exists("register_kernel_def", mode = "function")) {
  register_kernel_def(kernel_inverse_gaussian_def)
}
