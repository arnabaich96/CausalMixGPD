library(CausalMixGPD)
mcmc_fixed <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)
data("causal_pos500_p3_k2", package = "CausalMixGPD")
dat <- causal_pos500_p3_k2
df  <- data.frame(y = dat$y, A = dat$A, dat$X)
cfit <- dpmgpd.causal(
  formula = y ~ x1 + x2 + x3,
  data   = df,
  treat  = df$A,
  backend    = "sb",
  kernel     = "lognormal",
  components = 5,
  PS         = "logit",
  ps_scale   = "logit",
  ps_summary = "mean",
  mcmc_outcome = mcmc_fixed,
  mcmc_ps      = mcmc_fixed
)
print(class(cfit))
