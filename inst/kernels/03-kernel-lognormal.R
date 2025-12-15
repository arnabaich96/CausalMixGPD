# Kernel: Lognormal ----------------------------------------------------------
# Component-level API: d/p/q/r

kernel_lognormal_def <- list(
  name   = "lognormal",
  params = c("meanlog", "sdlog"),
  support = c(lower = 0, upper = Inf),
  default_trans = list(meanlog = "identity", sdlog = "exp"),

  d = function(y, meanlog, sdlog, log = FALSE) {
    stats::dlnorm(y, meanlog = meanlog, sdlog = sdlog, log = log)
  },

  p = function(q, meanlog, sdlog, lower.tail = TRUE, log.p = FALSE) {
    stats::plnorm(q, meanlog = meanlog, sdlog = sdlog, lower.tail = lower.tail, log.p = log.p)
  },

  q = function(p, meanlog, sdlog, lower.tail = TRUE, log.p = FALSE) {
    stats::qlnorm(p, meanlog = meanlog, sdlog = sdlog, lower.tail = lower.tail, log.p = log.p)
  },

  r = function(n, meanlog, sdlog) {
    stats::rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  }
)

if (exists("register_kernel_def", mode = "function")) {
  register_kernel_def(kernel_lognormal_def)
}
