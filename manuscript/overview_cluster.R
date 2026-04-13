# =============================================================================
# overview_cluster.R
# Illustrates the CausalMixGPD clustering (dpmix.cluster) API as shown in the
# Package overview section of CausalMixGPD_JSS_article.Rnw.
#
# IMPORTANT: In the Rnw, almost all chunks in this section are eval=FALSE —
# they are displayed as code listings but not executed.  The only chunk
# evaluated at compile time is overview-mcmc-settings (prints mcmc_fixed).
# All other blocks are wrapped in if (FALSE) here so this script runs cleanly
# as standalone R code without performing any long MCMC computation.
# =============================================================================

library(CausalMixGPD)

# -----------------------------------------------------------------------------
# MCMC settings — printed by the overview-mcmc-settings chunk (eval=TRUE).
# All model-fitting calls below use mcmc_fixed as their mcmc argument.
# niter:   total MCMC iterations
# nburnin: warm-up iterations discarded from posterior summaries
# thin:    thinning factor (1 = keep every sample)
# nchains: number of independent chains
# seed:    NIMBLE RNG seed for reproducibility
# -----------------------------------------------------------------------------
mcmc_fixed <- list(
  niter   = 2000,
  nburnin = 500,
  thin    = 1,
  nchains = 1,
  seed    = 2026
)

# ---- chunk: overview-mcmc-settings (eval=TRUE in Rnw) ----------------------
# The only chunk in this section that actually runs during Rnw compilation.
# Prints a tidy data frame of the MCMC configuration.
data.frame(mcmc_fixed, row.names = NULL)

# ---- chunk: overview-data-reload (eval=FALSE in Rnw) -----------------------
# Load a synthetic dataset bundled with the package.
# nc_posX100_p3_k2: n=100, p=3 covariates (x1, x2, x3), K=2 true components,
# outcomes on the positive real line.
# dat is the data.frame fed to formula-based wrappers.
if (FALSE) {
  data("nc_posX100_p3_k2", package = "CausalMixGPD")
  dat <- data.frame(y = nc_posX100_p3_k2$y, nc_posX100_p3_k2$X)
}

# ---- chunk: overview-fit-model1 (eval=FALSE in Rnw) ------------------------
# dpmgpd() fits a DP mixture with a spliced GPD tail.
# Formula syntax: response ~ predictor1 + predictor2 + ...
# Predictors enter both the bulk mixture weights and the GPD tail model.
# kernel must be one of "gamma", "lognormal", "invgauss", "normal", etc.
# backend selects the DP weight representation: "crp", "sb", or "spliced".
if (FALSE) {
  fit <- dpmgpd(
    formula = y ~ x1 + x2 + x3,
    data    = dat,
    mcmc    = mcmc_fixed
  )
}

# ---- chunk: overview-fit-model2 (eval=FALSE in Rnw) ------------------------
# dpmix() is the bulk-only companion to dpmgpd() — no GPD tail splicing.
# Use this when the response does not exhibit heavy tails or when a simpler
# bulk DP mixture suffices.
if (FALSE) {
  fit <- dpmix(
    formula = y ~ x1 + x2 + x3,
    data    = dat,
    mcmc    = mcmc_fixed
  )
}

# ---- chunk: overview-fit-diagnostics (eval=FALSE in Rnw) -------------------
# Post-fitting diagnostics and summaries:
# summary(fit)        — posterior mean, SD, and HPD intervals for all parameters
# params(fit)         — extract raw MCMC sample matrices
# plot(fit, "traceplot", params = "alpha") — traceplot for DP concentration
# plot(fit, "auto")   — automatic selection of appropriate diagnostic plots
if (FALSE) {
  summary(fit)
  params(fit)
  plot(fit, family = "traceplot", params = "alpha")
  plot(fit, family = "auto")
}

# ---- chunk: overview-estimation (eval=FALSE in Rnw) ------------------------
# Posterior predictive summaries over the training data:
# "density"  — evaluate the posterior predictive density f(y | x_i)
# "quantile" — return marginal quantile estimates at specified probabilities
# "mean"     — posterior mean of the predictive distribution
# interval options: "credible" (equal-tail) or "hpd" (shortest interval)
if (FALSE) {
  predict(fit, type = "density",  interval = "credible", level = 0.95)
  predict(fit, type = "quantile", index = c(0.25, 0.5, 0.75))
  predict(fit, type = "mean",     interval = "hpd",      level = 0.9)
}

# ---- chunk: overview-predict-quantile-prep (eval=FALSE in Rnw) -------------
# Construct a new-data grid and a response grid for survival/quantile queries.
# x_new: a data.frame of covariate values at which to evaluate the predictive
#         distribution — here at the quartiles of each predictor.
# y_grid: a fine grid of response values for evaluating survival P(Y > y | x).
if (FALSE) {
  x_new <- as.data.frame(
    lapply(
      dat[, c("x1", "x2", "x3")],
      quantile,
      probs = c(0.25, 0.50, 0.75),
      na.rm = TRUE
    )
  )
  y_grid <- seq(0, 10, length.out = 200)
}

# ---- chunk: overview-predict-quantile (eval=FALSE in Rnw) ------------------
# Out-of-sample predictive queries at new covariate values:
# "survival"  — P(Y > y_grid[j] | x_new[i, ]) for each (i, j) pair
# "quantile"  — Q(tau | x_new[i, ]) at specified probability levels tau
if (FALSE) {
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
}

# ---- chunk: cluster_example_data (eval=FALSE in Rnw) -----------------------
# Load the real-valued clustering dataset (n=100, p=3, K=2).
# Only the first 100 rows are retained here; rows 101-110 would be
# used as held-out new observations in cluster_label_new below.
if (FALSE) {
  data("nc_realX100_p3_k2", package = "CausalMixGPD")
  dat_cl <- data.frame(
    y = nc_realX100_p3_k2$y[1:100],
    nc_realX100_p3_k2$X[1:100, ]
  )
}

# ---- chunk: cluster_example_fit (eval=FALSE in Rnw) ------------------------
# dpmix.cluster() clusters observations via a DP mixture model.
# type = "both"    — covariates enter both mixture weights AND component params
# type = "weights" — covariates enter only the mixture weights (used in data analysis)
# The posterior produces soft cluster assignments summarised by a PSM.
if (FALSE) {
  fit_cluster <- dpmix.cluster(
    y ~ x1 + x2 + x3,
    data       = dat_cl[1:100, ],
    kernel     = "laplace",
    type       = "both",
    components = 10,
    mcmc       = mcmc_fixed
  )
}

# ---- chunk: cluster_psm (eval=FALSE in Rnw) ---------------------------------
# Compute the Posterior Similarity Matrix.
# predict(fit_cluster, type = "psm") returns an n x n matrix where
# entry (i, j) = P(z_i = z_j | data), averaged over MCMC iterations.
if (FALSE) {
  predict(fit_cluster, type = "psm")
}

# ---- chunk: cluster_label_new (eval=FALSE in Rnw) ---------------------------
# Classify new observations into training clusters.
# predict(..., newdata, type = "label") assigns each row of newdata to the
# nearest representative training cluster using the PSM-derived medoid partition.
# NOTE: dat_cl only has 100 rows (indices 1:100); this call is illustrative.
if (FALSE) {
  predict(
    fit_cluster,
    newdata = dat_cl[101:110, ],
    type    = "label"
  )
}
