# =============================================================================
# overview_cluster.R
# Package overview — clustering API (dpmgpd, dpmix, dpmix.cluster).
# Uses bundled synthetic datasets shipped with CausalMixGPD.
# =============================================================================

library(CausalMixGPD)

# MCMC settings used for all fits below.
mcmc_fixed <- list(
  niter   = 2000,
  nburnin = 500,
  thin    = 1,
  nchains = 1,
  seed    = 2026
)

data.frame(mcmc_fixed, row.names = NULL)

# =============================================================================
# Data
# nc_posX100_p3_k2: n=100, p=3 covariates, K=2 true components,
# outcomes on the positive real line.
# =============================================================================
data("nc_posX100_p3_k2", package = "CausalMixGPD")
dat <- data.frame(y = nc_posX100_p3_k2$y, nc_posX100_p3_k2$X)

# =============================================================================
# dpmgpd(): DP mixture with spliced GPD tail.
# backend: DP weight representation — "crp", "sb", or "spliced".
# kernel:  bulk component family — "gamma", "lognormal", "normal", etc.
# =============================================================================
fit <- dpmgpd(
  formula = y ~ x1 + x2 + x3,
  data    = dat,
  backend = "crp",
  kernel  = "gamma",
  mcmc    = mcmc_fixed
)

# =============================================================================
# dpmix(): bulk-only DP mixture, no GPD tail splicing.
# =============================================================================
fit <- dpmix(
  formula = y ~ x1 + x2 + x3,
  data    = dat,
  backend = "crp",
  kernel  = "gamma",
  mcmc    = mcmc_fixed
)

# =============================================================================
# Diagnostics
# =============================================================================
summary(fit)
params(fit)
plot(fit, family = "traceplot", params = "alpha")
plot(fit, family = "auto")

# =============================================================================
# In-sample posterior predictive summaries
# =============================================================================
predict(fit, type = "density",  interval = "credible", level = 0.95)
predict(fit, type = "quantile", index = c(0.25, 0.5, 0.75))
predict(fit, type = "mean",     interval = "hpd",      level = 0.9)

# =============================================================================
# Out-of-sample prediction
# x_new: covariate grid at each predictor's quartiles (3 rows).
# y_grid: response grid for evaluating P(Y > y | x).
# =============================================================================
x_new <- as.data.frame(
  lapply(
    dat[, c("x1", "x2", "x3")],
    quantile,
    probs = c(0.25, 0.50, 0.75),
    na.rm = TRUE
  )
)
y_grid <- seq(0, 10, length.out = 200)

predict(
  fit,
  newdata  = x_new,
  y        = y_grid,
  type     = "survival",
  level    = 0.95
)
predict(
  fit,
  newdata  = x_new,
  type     = "quantile",
  index    = c(0.5, 0.99),
  interval = "credible"
)

# =============================================================================
# Clustering
# nc_realX100_p3_k2: n=100, p=3, K=2, real-valued outcomes.
# Rows 1:90 used for fitting; rows 91:100 used as held-out new observations.
# =============================================================================
data("nc_realX100_p3_k2", package = "CausalMixGPD")
dat_cl <- data.frame(
  y = nc_realX100_p3_k2$y,
  nc_realX100_p3_k2$X
)

# dpmix.cluster(): DP mixture clustering with covariate-dependent weights.
#   type = "both"    — covariates enter mixture weights and component params
#   type = "weights" — covariates enter mixture weights only
fit_cluster <- dpmix.cluster(
  y ~ x1 + x2 + x3,
  data       = dat_cl[1:90, ],
  kernel     = "laplace",
  type       = "both",
  components = 10,
  mcmc       = mcmc_fixed
)

# Posterior similarity matrix: PSM[i,j] = P(z_i = z_j | data)
predict(fit_cluster, type = "psm")

# Classify held-out observations into training clusters
predict(
  fit_cluster,
  newdata = dat_cl[91:100, ],
  type    = "label"
)
