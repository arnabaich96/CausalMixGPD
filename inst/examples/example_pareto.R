# Example: pareto kernel, response-only

set.seed(2)
# Simple heavy-tailed sample
y <- 1 / runif(300)  # not true Pareto but heavy-ish tail

fit <- DPmixGPD::fit_mixgpd_xy(
  Y      = y,
  X      = NULL,
  kernel = "pareto",
  dp_rep = "stick_breaking",
  dp_ctrl = list(K = 5),
  mcmc   = list(n_iter = 1500, burn_in = 500, chains = 1),
  alpha  = 0.1
)

print(fit)
summary(fit)
as_mcmc(fit)
