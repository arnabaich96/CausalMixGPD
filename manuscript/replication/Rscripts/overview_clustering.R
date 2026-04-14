# =============================================================================
# overview_clustering.R
# Package overview — clustering API (dpmix.cluster, predict).
# Uses bundled synthetic data shipped with CausalMixGPD.
# =============================================================================

library(CausalMixGPD)

# MCMC settings used for the clustering fit below.
mcmc_fixed <- list(
  niter   = 2000,
  nburnin = 500,
  thin    = 1,
  nchains = 1,
  seed    = 2026
)

# Clustering data
# nc_realX100_p3_k2: n=100, p=3, K=2, real-valued outcomes.
# Rows 1:90 are used for fitting; rows 91:100 are held out.
data("nc_realX100_p3_k2", package = "CausalMixGPD")
dat_cl <- data.frame(
  y = nc_realX100_p3_k2$y,
  nc_realX100_p3_k2$X
)

# dpmix.cluster(): DP mixture clustering with covariate-dependent weights.
# type = "both" means covariates enter mixture weights and component parameters.
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
