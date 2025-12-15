# Kernel: Amoroso -------------------------------------------------------------
# Amoroso distribution (generalized gamma with location).
# Parametrization used here:
#   location = a (real, but for positive-support uses a >= 0 typically)
#   scale    = b > 0
#   shape1   = k > 0
#   shape2   = theta != 0 (we assume theta > 0 for monotone transform)
#
# If theta > 0 and x > a:
#   T = ((x - a)/b)^theta ~ Gamma(shape = k, rate = 1)
#   CDF: P(X <= x) = pgamma(T, k, 1)
#   Quantile: x = a + b * qgamma(p, k, 1)^(1/theta)
#
# Note: theta < 0 corresponds to reversed tails; we do not implement it here.

.kernel_amoroso_check <- function(location, scale, shape1, shape2) {
  if (any(!is.finite(location))) stop("Amoroso: non-finite location.", call. = FALSE)
  if (any(!is.finite(scale)) || any(scale <= 0)) stop("Amoroso: scale must be > 0.", call. = FALSE)
  if (any(!is.finite(shape1)) || any(shape1 <= 0)) stop("Amoroso: shape1 must be > 0.", call. = FALSE)
  if (any(!is.finite(shape2)) || any(shape2 <= 0)) stop("Amoroso: shape2 must be > 0 (theta).", call. = FALSE)
}

kernel_amoroso_def <- list(
  name   = "amoroso",
  params = c("location", "scale", "shape1", "shape2"),
  support = c(lower = 0, upper = Inf),
  default_trans = list(location = "identity", scale = "exp", shape1 = NULL, shape2 = NULL),

  d = function(y, location, scale, shape1, shape2, log = FALSE) {
    .kernel_amoroso_check(location, scale, shape1, shape2)
    x <- y
    a <- location; b <- scale; k <- shape1; th <- shape2
    out <- rep(if (log) -Inf else 0, length(x))
    ok <- x > a
    if (!any(ok)) return(out)
    z <- (x[ok] - a) / b
    t <- z^th
    # log pdf: log(th) - log(b) - lgamma(k) + (k*th - 1)log(z) - t
    logpdf <- log(th) - log(b) - lgamma(k) + (k * th - 1) * log(z) - t
    out[ok] <- if (log) logpdf else exp(logpdf)
    out
  },

  p = function(q, location, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
    .kernel_amoroso_check(location, scale, shape1, shape2)
    x <- q
    a <- location; b <- scale; k <- shape1; th <- shape2
    cdf <- numeric(length(x))
    cdf[x <= a] <- 0
    ok <- x > a
    if (any(ok)) {
      z <- (x[ok] - a) / b
      t <- z^th
      cdf[ok] <- stats::pgamma(t, shape = k, rate = 1)
    }
    if (!lower.tail) cdf <- 1 - cdf
    if (log.p) return(log(cdf))
    cdf
  },

  q = function(p, location, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
    .kernel_amoroso_check(location, scale, shape1, shape2)
    if (log.p) p <- exp(p)
    if (!lower.tail) p <- 1 - p
    if (any(p <= 0 | p >= 1, na.rm = TRUE)) stop("Amoroso: p must be in (0,1).", call. = FALSE)
    a <- location; b <- scale; k <- shape1; th <- shape2
    t <- stats::qgamma(p, shape = k, rate = 1)
    a + b * (t)^(1 / th)
  },

  r = function(n, location, scale, shape1, shape2) {
    .kernel_amoroso_check(location, scale, shape1, shape2)
    a <- location; b <- scale; k <- shape1; th <- shape2
    t <- stats::rgamma(n, shape = k, rate = 1)
    a + b * (t)^(1 / th)
  }
)

if (exists("register_kernel_def", mode = "function")) {
  register_kernel_def(kernel_amoroso_def)
}
