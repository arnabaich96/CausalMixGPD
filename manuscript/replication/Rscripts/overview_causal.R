# =============================================================================
# overview_causal.R
# Package overview — causal inference API (dpmgpd.causal, qtt, predict).
# Uses bundled synthetic causal dataset shipped with CausalMixGPD.
# =============================================================================

library(CausalMixGPD)
cmgpd_seed <- 2026

# MCMC settings used for both the outcome models and the PS model.
mcmc_fixed <- list(
  niter   = 2000,
  nburnin = 500,
  thin    = 1,
  nchains = 1,
  seed    = cmgpd_seed
)

data.frame(mcmc_fixed, row.names = NULL)

# =============================================================================
# Data
# causal_pos500_p3_k2: n=500, p=3 covariates, K=2 true components,
# positive-support outcomes, binary treatment indicator A.
# =============================================================================
data("causal_pos500_p3_k2", package = "CausalMixGPD")
causal_dat <- causal_pos500_p3_k2
causal_df  <- data.frame(y = causal_dat$y, A = causal_dat$A, causal_dat$X)

# =============================================================================
# dpmgpd.causal(): arm-specific DP mixture + GPD tail models.
#   formula / data / treat — response, covariates, treatment column
#   backend                — "crp", "sb", or "spliced"
#   kernel                 — bulk component family: "laplace", "gamma", etc.
#   PS                     — propensity-score model: "logit", "probit", FALSE
#   ps_scale               — PS augmentation scale ("logit" = log-odds)
#   ps_summary             — PS posterior summary: "mean" or "median"
#   mcmc_outcome           — MCMC settings for the two outcome-arm models
#   mcmc_ps                — MCMC settings for the PS model
# =============================================================================
causal_fit <- dpmgpd.causal(
  formula      = y ~ x1 + x2 + x3,
  data         = causal_df,
  treat        = "A",
  backend      = "crp",
  kernel       = "laplace",
  components   = 5,
  PS           = "logit",
  ps_scale     = "logit",
  ps_summary   = "mean",
  mcmc_outcome = mcmc_fixed,
  mcmc_ps      = mcmc_fixed
)

# =============================================================================
# qtt(): Quantile Treatment effect on the Treated.
#   QTT(tau) = Q_1(tau | A=1) - Q_0(tau | A=1),
#   marginalised over the treated subpopulation covariate distribution.
# =============================================================================
qtt_fit <- qtt(
  causal_fit,
  probs    = c(0.50, 0.90, 0.95),
  interval = "credible"
)
summary(qtt_fit)
plot(qtt_fit, type = "effect")

# =============================================================================
# Out-of-sample conditional survival prediction.
# causal_xgrid: all predictor-quartile combinations (3^3 = 27 rows).
# predict() with type = "survival": P(Y(a) > y | X = x) for each row.
# Align columns to training design matrix; optional id avoids length mismatch.
# =============================================================================
causal_xgrid <- expand.grid(lapply(
  causal_df[c("x1", "x2", "x3")],
  quantile,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
))
causal_xgrid <- as.matrix(causal_xgrid)
causal_xgrid <- causal_xgrid[, colnames(causal_fit$bundle$data$X), drop = FALSE]

predict(causal_fit,
           newdata = causal_xgrid,
           type    = "survival",
             y       = rep(4,length=nrow(causal_xgrid)))




