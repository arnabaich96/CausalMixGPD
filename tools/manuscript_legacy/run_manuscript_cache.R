options(width = 70)

library(CausalMixGPD)
library(MASS)

mcmc_fixed <- list(
  niter = 200,
  nburnin = 50,
  thin = 1,
  nchains = 1,
  seed = 1
)

cache_dir <- "cache"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Cache 1: one-arm spliced DP--GPD fit
# ------------------------------------------------------------
data("nc_posX100_p3_k2", package = "CausalMixGPD")
dat_overview <- data.frame(y = nc_posX100_p3_k2$y, nc_posX100_p3_k2$X)

fit <- dpmgpd(
  formula = y ~ x1 + x2 + x3,
  data = dat_overview,
  backend = "sb",
  kernel = "lognormal",
  components = 5,
  mcmc = mcmc_fixed
)

saveRDS(fit, file.path(cache_dir, "overview_fit.rds"))

# ------------------------------------------------------------
# Cache 2: causal fit
# ------------------------------------------------------------
data("causal_pos500_p3_k2", package = "CausalMixGPD")
dat_causal <- causal_pos500_p3_k2
df_causal <- data.frame(y = dat_causal$y, A = dat_causal$A, dat_causal$X)

cfit <- dpmgpd(
  formula = y ~ x1 + x2 + x3,
  data = df_causal,
  treat = df_causal$A,
  backend = "sb",
  kernel = "lognormal",
  components = 5,
  PS = "logit",
  ps_scale = "logit",
  ps_summary = "mean",
  mcmc_outcome = mcmc_fixed,
  mcmc_ps = mcmc_fixed
)

saveRDS(cfit, file.path(cache_dir, "causal_fit.rds"))

# ------------------------------------------------------------
# Cache 3: CRP cluster fit
# ------------------------------------------------------------
data("Boston", package = "MASS")
dat_cluster <- Boston

fit_clust <- dpmix(
  formula = crim ~ lstat + rm + nox,
  data = dat_cluster,
  backend = "crp",
  kernel = "lognormal",
  components = 3,
  mcmc = mcmc_fixed
)

saveRDS(fit_clust, file.path(cache_dir, "app_cluster_fit.rds"))

print("Cache build complete")
print(file.path(cache_dir, c("overview_fit.rds", "causal_fit.rds", "app_cluster_fit.rds")))
