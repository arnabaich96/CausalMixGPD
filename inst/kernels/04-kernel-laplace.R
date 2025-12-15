# Kernel: Laplace (double-exponential) ---------------------------------------
# Parametrization: location (mu), scale (b>0)
# Component-level API: d/p/q/r

.kernel_laplace_check <- function(location, scale) {
  if (any(!is.finite(location))) stop("Laplace: non-finite location.", call. = FALSE)
  if (any(!is.finite(scale)) || any(scale <= 0)) stop("Laplace: scale must be > 0.", call. = FALSE)
}

kernel_laplace_def <- list(
  name   = "laplace",
  params = c("location", "scale"),
  support = c(lower = -Inf, upper = Inf),
  default_trans = list(location = "identity", scale = "exp"),

  d = function(y, location, scale, log = FALSE) {
    .kernel_laplace_check(location, scale)
    z <- abs(y - location) / scale
    if (log) return(-log(2 * scale) - z)
    (1 / (2 * scale)) * exp(-z)
  },

  p = function(q, location, scale, lower.tail = TRUE, log.p = FALSE) {
    .kernel_laplace_check(location, scale)
    z <- (q - location) / scale
    cdf <- ifelse(z < 0, 0.5 * exp(z), 1 - 0.5 * exp(-z))
    if (!lower.tail) cdf <- 1 - cdf
    if (log.p) return(log(cdf))
    cdf
  },

  q = function(p, location, scale, lower.tail = TRUE, log.p = FALSE) {
    .kernel_laplace_check(location, scale)
    if (log.p) p <- exp(p)
    if (!lower.tail) p <- 1 - p
    if (any(p <= 0 | p >= 1, na.rm = TRUE)) stop("Laplace: p must be in (0,1).", call. = FALSE)
    ifelse(p < 0.5,
           location + scale * log(2 * p),
           location - scale * log(2 * (1 - p)))
  },

  r = function(n, location, scale) {
    .kernel_laplace_check(location, scale)
    u <- stats::runif(n)
    ifelse(u < 0.5,
           location + scale * log(2 * u),
           location - scale * log(2 * (1 - u)))
  }
)

if (exists("register_kernel_def", mode = "function")) {
  register_kernel_def(kernel_laplace_def)
}
