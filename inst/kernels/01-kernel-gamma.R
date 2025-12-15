# Kernel: Gamma ---------------------------------------------------------------
# Component-level API: d/p/q/r

kernel_gamma_def <- list(
  name   = "gamma",
  params = c("shape", "scale"),
  support = c(lower = 0, upper = Inf),
  default_trans = list(shape = NULL, scale = "exp"),

  d = function(y, shape, scale, log = FALSE) {
    stats::dgamma(y, shape = shape, scale = scale, log = log)
  },

  p = function(q, shape, scale, lower.tail = TRUE, log.p = FALSE) {
    stats::pgamma(q, shape = shape, scale = scale, lower.tail = lower.tail, log.p = log.p)
  },

  q = function(p, shape, scale, lower.tail = TRUE, log.p = FALSE) {
    stats::qgamma(p, shape = shape, scale = scale, lower.tail = lower.tail, log.p = log.p)
  },

  r = function(n, shape, scale) {
    stats::rgamma(n, shape = shape, scale = scale)
  }
)

if (exists("register_kernel_def", mode = "function")) {
  register_kernel_def(kernel_gamma_def)
}
