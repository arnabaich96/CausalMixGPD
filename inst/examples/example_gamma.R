# Example: gamma kernel, no GPD tail

set.seed(1)
dat <- data.frame(
  y = rgamma(200, shape = 2, scale = 1),
  x = rnorm(200)
)

fit <- DPmixGPD::fit_mixgpd(
  y ~ x,
  data   = dat,
  kernel = "gamma",
  dp_rep = "stick_breaking",
  dp_ctrl = list(K = 5),
  trans  = list(
    scale = "exp"  # default, but shown explicitly
  ),
  mcmc   = list(n_iter = 2000, burn_in = 1000, chains = 2, parallel = FALSE),
  alpha  = 0.05
)

print(fit)
summary(fit)
coef(fit)
mcmc_ggdiag(fit, what = "trace")
