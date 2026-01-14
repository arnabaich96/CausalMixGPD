## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 8,
  fig.height = 6,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)
library(ggplot2)
set.seed(123)

## ----data-setup---------------------------------------------------------------
# set.seed(42)
# n <- 200
# 
# # Covariates
# X <- cbind(
#   x1 = rnorm(n, mean = 0, sd = 1),
#   x2 = rnorm(n, mean = 0, sd = 1)
# )
# 
# # Treatment (binary)
# T <- rbinom(n, size = 1, prob = 0.5)
# 
# # Outcome: function of treatment, covariates, and PS
# ps_prob <- 0.5  # True PS model (simplified)
# y <- ifelse(
#   T == 1,
#   rnorm(n, mean = 3 + 0.5 * X[, 1], sd = 1.2),  # Treatment arm
#   rnorm(n, mean = 1 + 0.3 * X[, 1], sd = 1.0)   # Control arm
# )
# 
# df_causal <- data.frame(
#   y = y,
#   T = as.factor(T),
#   x1 = X[, 1],
#   x2 = X[, 2]
# )
# 
# # Visualization
# p_raw <- ggplot(df_causal, aes(x = x1, y = y, color = T)) +
#   geom_point(alpha = 0.6) +
#   scale_color_manual(values = c("0" = "steelblue", "1" = "red")) +
#   labs(title = "Outcome by Treatment & Covariate", x = "X1", y = "y", color = "Treatment") +
#   theme_minimal()
# 
# print(p_raw)
# 
# cat("Sample size:", n, "\n")
# cat("Treatment:", table(T), "\n")
# cat("Mean outcome (T=0):", mean(y[T == 0]), "\n")
# cat("Mean outcome (T=1):", mean(y[T == 1]), "\n")

## ----causal-spec--------------------------------------------------------------
# # Build causal bundle with PS modeling
# bundle_causal_crp <- build_causal_bundle(
#   y = y,
#   T = T,
#   X = X,
#   kernel = "normal",                # Outcome kernel
#   backend = "crp",                  # CRP for outcome
#   PS = "logit",                     # Propensity score: logit model
#   GPD = FALSE,
#   Kmax = 10,
#   mcmc = list(
#     niter = 2000,
#     nburnin = 500,
#     nchains = 2,
#     thin = 1
#   )
# )
# 
# cat("Causal bundle (CRP + logit PS) created.\n")

## ----ps-logit-----------------------------------------------------------------
# # Logit: Default, flexible
# bundle_logit <- build_causal_bundle(
#   y = y, T = T, X = X,
#   kernel = "normal", backend = "crp",
#   PS = "logit",           # Bayesian logit via DP mixture
#   GPD = FALSE, Kmax = 10,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# 
# fit_logit <- run_mcmc_bundle_manual(bundle_logit)
# cat("\n=== Logit PS Model ===\n")
# summary(fit_logit)

## ----ps-probit----------------------------------------------------------------
# bundle_probit <- build_causal_bundle(
#   y = y, T = T, X = X,
#   kernel = "normal", backend = "crp",
#   PS = "probit",          # Bayesian probit
#   GPD = FALSE, Kmax = 10,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# 
# fit_probit <- run_mcmc_bundle_manual(bundle_probit)
# cat("\n=== Probit PS Model ===\n")
# summary(fit_probit)

## ----ps-naive-----------------------------------------------------------------
# # Naive: No PS correction (treatment as covariate only)
# bundle_naive <- build_causal_bundle(
#   y = y, T = T, X = X,
#   kernel = "normal", backend = "crp",
#   PS = "naive",           # No PS model
#   GPD = FALSE, Kmax = 10,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# 
# fit_naive <- run_mcmc_bundle_manual(bundle_naive)
# cat("\n=== Naive PS Model (No PS Adjustment) ===\n")
# summary(fit_naive)

## ----ate----------------------------------------------------------------------
# # Extract treatment effect from fit
# cat("\n=== TREATMENT EFFECTS ===\n")
# 
# cat("\nLogit PS Model:\n")
# ate_logit <- coef(fit_logit)
# cat("Posterior mean treatment effect:", ate_logit, "\n")
# 
# cat("\nProbit PS Model:\n")
# ate_probit <- coef(fit_probit)
# cat("Posterior mean treatment effect:", ate_probit, "\n")
# 
# cat("\nNaive Model:\n")
# ate_naive <- coef(fit_naive)
# cat("Posterior mean treatment effect:", ate_naive, "\n")

## ----cate---------------------------------------------------------------------
# # CATE: ATE conditional on covariates
# X_cate <- cbind(x1 = c(-1, 0, 1), x2 = 0)
# 
# cate_logit <- predict(fit_logit, newdata = X_cate, type = "treatment.effect")
# cat("CATE at X1 = -1, 0, 1 (Logit PS):\n")
# print(cate_logit)

## ----ps-comparison------------------------------------------------------------
# cat("\n=== MODEL COMPARISON ===\n")
# 
# df_comp <- data.frame(
#   Model = c("Logit PS", "Probit PS", "Naive"),
#   Comparison = "See summary()",
#   ATE = c(ate_logit, ate_probit, ate_naive)
# )
# 
# print(df_comp)
# 
# cat("\n✓ Logit typically preferred for binary outcome in treatment\n")

## ----ps-dist------------------------------------------------------------------
# # Extract estimated PS values
# ps_fitted <- fitted(fit_logit, type = "ps")
# 
# df_ps <- data.frame(ps = ps_fitted, T = as.factor(T))
# 
# p_ps <- ggplot(df_ps, aes(x = ps, fill = T)) +
#   geom_histogram(alpha = 0.6, bins = 30) +
#   scale_fill_manual(values = c("0" = "steelblue", "1" = "red")) +
#   labs(title = "Estimated Propensity Score Distribution", x = "PS", y = "Count") +
#   theme_minimal()
# 
# print(p_ps)

## ----overlap------------------------------------------------------------------
# # PS overlap: critical for causal inference
# ps_control <- ps_fitted[T == 0]
# ps_treated <- ps_fitted[T == 1]
# 
# p_overlap <- ggplot(data.frame(ps = ps_fitted, T = as.factor(T)),
#                      aes(x = ps, fill = T)) +
#   geom_density(alpha = 0.5) +
#   scale_fill_manual(values = c("0" = "steelblue", "1" = "red")) +
#   labs(title = "PS Overlap: Common Support", x = "PS", y = "Density") +
#   theme_minimal()
# 
# print(p_overlap)
# 
# cat("Common support check:\n")
# cat("  Control PS range: [", min(ps_control), ",", max(ps_control), "]\n")
# cat("  Treated PS range: [", min(ps_treated), ",", max(ps_treated), "]\n")

## ----residuals----------------------------------------------------------------
# resid_vals <- residuals(fit_logit)
# 
# p_resid <- ggplot(data.frame(r = resid_vals, T = as.factor(T)),
#                    aes(x = T, y = r, fill = T)) +
#   geom_boxplot(alpha = 0.6) +
#   geom_jitter(alpha = 0.3, width = 0.2) +
#   scale_fill_manual(values = c("0" = "steelblue", "1" = "red")) +
#   labs(title = "Residuals by Treatment", x = "Treatment", y = "Residual") +
#   theme_minimal()
# 
# print(p_resid)

