# =============================================================================
# overview_causal.R
# Illustrates the CausalMixGPD causal inference API as shown in the Package
# overview section of CausalMixGPD_JSS_article.Rnw.
#
# IMPORTANT: All chunks in this section are eval=FALSE in the Rnw — they are
# displayed as code listings but not executed at compile time.  The only
# runnable chunk is overview-mcmc-settings (prints mcmc_fixed).
# All model-fitting and estimand blocks are wrapped in if (FALSE) so this
# script runs cleanly as standalone R code without any MCMC computation.
# =============================================================================

library(CausalMixGPD)

# -----------------------------------------------------------------------------
# MCMC settings — printed by the overview-mcmc-settings chunk (eval=TRUE).
# mcmc_outcome: MCMC configuration for the two outcome-arm models (control/treated).
# mcmc_ps:      MCMC configuration for the propensity-score logistic model.
# Both may share the same settings list, as done here.
# -----------------------------------------------------------------------------
mcmc_fixed <- list(
  niter   = 2000,
  nburnin = 500,
  thin    = 1,
  nchains = 1,
  seed    = 2026
)

# ---- chunk: overview-mcmc-settings (eval=TRUE in Rnw) ----------------------
# Prints a tidy data frame of the MCMC configuration — the only chunk in this
# section that actually executes during Rnw compilation.
data.frame(mcmc_fixed, row.names = NULL)

# ---- chunk: overview-causal-data (eval=FALSE in Rnw) -----------------------
# Load a synthetic causal dataset bundled with the package.
# causal_pos500_p3_k2: n=500, p=3 covariates, K=2 true mixing components,
# positive-support outcomes, binary treatment indicator A.
# causal_df is the data.frame consumed by formula-based causal wrappers.
if (FALSE) {
  data("causal_pos500_p3_k2", package = "CausalMixGPD")
  causal_dat <- causal_pos500_p3_k2
  causal_df  <- data.frame(y = causal_dat$y, A = causal_dat$A, causal_dat$X)
}

# ---- chunk: overview-causal-fit-model (eval=FALSE in Rnw) ------------------
# dpmgpd.causal() fits separate arm-specific DP mixture + spliced GPD tail
# models for treated (A=1) and control (A=0) outcome distributions.
#
# Key arguments:
#   formula / data / treat — identify response, covariates, and treatment column
#   backend               — DP weight representation ("crp", "sb", or "spliced")
#   kernel                — bulk mixture kernel ("laplace", "gamma", etc.)
#   components            — DP truncation level
#   PS                    — propensity score model ("logit", "probit", or FALSE)
#   ps_scale              — scale for PS augmentation ("logit" = log-odds)
#   ps_summary            — posterior summary for PS ("mean" or "median")
#   mcmc_outcome          — MCMC settings for the outcome models
#   mcmc_ps               — MCMC settings for the PS model
if (FALSE) {
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
}

# ---- chunk: overview-causal-effects (eval=FALSE in Rnw) --------------------
# Causal estimand functions (all return S3 objects with summary/plot methods):
#
# qtt()     — Quantile Treatment effect on the Treated:
#               QTT(tau) = Q_1(tau | A=1) - Q_0(tau | A=1)
#             (marginalised over the treated subpopulation's covariate distribution)
# summary() — tabular summary of point estimates and credible intervals
# plot()    — visualise the QTT curve and arm-specific quantile functions
if (FALSE) {
  qtt <- qtt(
    causal_fit,
    probs    = c(0.50, 0.90, 0.95),
    interval = "credible"
  )
  summary(qtt)
  plot(qtt, type = "effect")
}

# ---- chunk: overview-causal-predict-contrast-prep (eval=FALSE in Rnw) ------
# Build a grid of new covariate values for out-of-sample prediction.
# expand.grid() creates all combinations of the three predictor quartiles —
# 3^3 = 27 rows.  Used to evaluate conditional survival P(Y > y | x, A=a).
if (FALSE) {
  causal_xgrid <- expand.grid(lapply(causal_df[c("x1", "x2", "x3")],
    quantile, probs = c(0.25, 0.50, 0.75)))
}

# ---- chunk: overview-causal-predict-contrast (eval=FALSE in Rnw) -----------
# predict() with type = "survival" evaluates P(Y(a) > y | X = x) for each
# row of newdata.  Here y is fixed at 4 (threshold in original data units).
# Returns a data frame with arm-specific survival probabilities and intervals.
if (FALSE) {
  predict(
    causal_fit,
    newdata = causal_xgrid,
    type    = "survival",
    y       = rep(4, nrow(causal_xgrid))
  )
}
