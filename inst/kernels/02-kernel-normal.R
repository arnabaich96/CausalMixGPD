# Kernel: Normal --------------------------------------------------------------
# Component-level API: d/p/q/r

kernel_normal_def <- list(
  name   = "normal",
  params = c("mean", "sd"),
  support = c(lower = -Inf, upper = Inf),
  default_trans = list(mean = "identity", sd = "exp"),

  d = function(y, mean, sd, log = FALSE) {
    stats::dnorm(y, mean = mean, sd = sd, log = log)
  },

  p = function(q, mean, sd, lower.tail = TRUE, log.p = FALSE) {
    stats::pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p)
  },

  q = function(p, mean, sd, lower.tail = TRUE, log.p = FALSE) {
    stats::qnorm(p, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p)
  },

  r = function(n, mean, sd) {
    stats::rnorm(n, mean = mean, sd = sd)
  }
)

if (exists("register_kernel_def", mode = "function")) {
  register_kernel_def(kernel_normal_def)
}
