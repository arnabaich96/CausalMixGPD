# Kernel: Pareto (Type I) -----------------------------------------------------
# Parametrization: scale = xm > 0, shape = alpha > 0
# Support: x >= xm
#
# CDF: 1 - (xm/x)^alpha
# Quantile: xm / (1-p)^(1/alpha)

.kernel_pareto_check <- function(scale, shape) {
  if (any(!is.finite(scale)) || any(scale <= 0)) stop("Pareto: scale must be > 0.", call. = FALSE)
  if (any(!is.finite(shape)) || any(shape <= 0)) stop("Pareto: shape must be > 0.", call. = FALSE)
}

kernel_pareto_def <- list(
  name   = "pareto",
  params = c("scale", "shape"),
  support = c(lower = 0, upper = Inf),
  default_trans = list(scale = "exp", shape = NULL),

  d = function(y, scale, shape, log = FALSE) {
    .kernel_pareto_check(scale, shape)
    x <- y; xm <- scale; a <- shape
    out <- rep(if (log) -Inf else 0, length(x))
    ok <- x >= xm
    if (!any(ok)) return(out)
    logpdf <- log(a) + a * log(xm) - (a + 1) * log(x[ok])
    out[ok] <- if (log) logpdf else exp(logpdf)
    out
  },

  p = function(q, scale, shape, lower.tail = TRUE, log.p = FALSE) {
    .kernel_pareto_check(scale, shape)
    x <- q; xm <- scale; a <- shape
    cdf <- numeric(length(x))
    cdf[x < xm] <- 0
    ok <- x >= xm
    if (any(ok)) cdf[ok] <- 1 - (xm / x[ok])^a
    if (!lower.tail) cdf <- 1 - cdf
    if (log.p) return(log(cdf))
    cdf
  },

  q = function(p, scale, shape, lower.tail = TRUE, log.p = FALSE) {
    .kernel_pareto_check(scale, shape)
    if (log.p) p <- exp(p)
    if (!lower.tail) p <- 1 - p
    if (any(p <= 0 | p >= 1, na.rm = TRUE)) stop("Pareto: p must be in (0,1).", call. = FALSE)
    xm <- scale; a <- shape
    xm / (1 - p)^(1 / a)
  },

  r = function(n, scale, shape) {
    .kernel_pareto_check(scale, shape)
    u <- stats::runif(n)
    xm <- scale; a <- shape
    xm / (1 - u)^(1 / a)
  }
)

if (exists("register_kernel_def", mode = "function")) {
  register_kernel_def(kernel_pareto_def)
}
