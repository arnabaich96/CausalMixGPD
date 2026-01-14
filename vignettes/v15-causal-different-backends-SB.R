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
# X <- cbind(
#   x1 = rnorm(n, mean = 0, sd = 1),
#   x2 = rnorm(n, mean = 0, sd = 1)
# )
# 
# T <- rbinom(n, size = 1, prob = 0.5)
# 
# y <- ifelse(
#   T == 1,
#   rnorm(n, mean = 3 + 0.5 * X[, 1], sd = 1.2),
#   rnorm(n, mean = 1 + 0.3 * X[, 1], sd = 1.0)
# )
# 
# cat("Sample size:", n, "\n")

## ----sb-ps-crp-outcome--------------------------------------------------------
# bundle_sb_crp <- build_causal_bundle(
#   y = y,
#   T = T,
#   X = X,
#   kernel = "normal",
#   backend = "crp",           # Outcome: CRP (flexible)
#   PS = "logit",              # PS: via SB (fixed components)
#   GPD = FALSE,
#   Kmax = 10,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# 
# fit_sb_crp <- run_mcmc_bundle_manual(bundle_sb_crp)
# 
# ate_sb_crp <- coef(fit_sb_crp)
# cat("=== SB PS + CRP Outcome ===\n")
# cat("ATE:", ate_sb_crp, "\n")

## ----sb-ps-sb-outcome---------------------------------------------------------
# bundle_sb_sb <- build_causal_bundle(
#   y = y,
#   T = T,
#   X = X,
#   kernel = "normal",
#   backend = "sb",            # Outcome: SB (fixed)
#   PS = "logit",              # PS: via SB
#   GPD = FALSE,
#   J = 10,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# 
# fit_sb_sb <- run_mcmc_bundle_manual(bundle_sb_sb)
# 
# ate_sb_sb <- coef(fit_sb_sb)
# cat("\n=== SB PS + SB Outcome ===\n")
# cat("ATE:", ate_sb_sb, "\n")

## ----all-combinations---------------------------------------------------------
# # CRP PS + CRP Outcome (from vignette 14)
# bundle_crp_crp <- build_causal_bundle(
#   y = y, T = T, X = X,
#   kernel = "normal", backend = "crp",
#   PS = "logit", GPD = FALSE, Kmax = 10,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# fit_crp_crp <- run_mcmc_bundle_manual(bundle_crp_crp)
# ate_crp_crp <- coef(fit_crp_crp)
# # CRP PS + SB Outcome (from vignette 14)
# bundle_crp_sb <- build_causal_bundle(
#   y = y, T = T, X = X,
#   kernel = "normal", backend = "sb",
#   PS = "logit", GPD = FALSE, J = 10,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# fit_crp_sb <- run_mcmc_bundle_manual(bundle_crp_sb)
# ate_crp_sb <- coef(fit_crp_sb)
# # Summary table
# df_all <- data.frame(
#   PS_Backend = rep(c("CRP", "SB"), each = 2),
#   Outcome_Backend = rep(c("CRP", "SB"), times = 2),
#   ATE = c(ate_crp_crp, ate_crp_sb, ate_sb_crp, ate_sb_sb),
#   Comparison = "See summary()"
# )
# 
# cat("\n=== ALL 4 BACKEND COMBINATIONS ===\n")
# print(df_all)
# 
# # Visualization
# p_all_ate <- ggplot(df_all, aes(x = paste(PS_Backend, Outcome_Backend, sep="+"),
#                                 y = ATE, fill = PS_Backend)) +
#   geom_col(alpha = 0.7) +
#   scale_fill_manual(values = c("CRP" = "steelblue", "SB" = "coral")) +
#   labs(title = "ATE Across Backend Combinations", x = "Backend Combo", y = "ATE") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# print(p_all_ate)

## ----ps-----------------------------------------------------------------------
# ps_fitted_sb <- fitted(fit_sb_crp, type = "ps")
# 
# df_ps <- data.frame(ps = ps_fitted_sb, T = as.factor(T))
# 
# p_ps <- ggplot(df_ps, aes(x = ps, fill = T)) +
#   geom_histogram(alpha = 0.6, bins = 30) +
#   scale_fill_manual(values = c("0" = "steelblue", "1" = "red")) +
#   labs(title = "PS Distribution: SB Backend", x = "PS", y = "Count") +
#   theme_minimal()
# 
# print(p_ps)

## ----hte----------------------------------------------------------------------
# X_hte <- cbind(x1 = seq(-2, 2, by = 0.5), x2 = 0)
# 
# # Extract CATE from all fits
# cate_crp_crp <- predict(fit_crp_crp, newdata = X_hte, type = "treatment.effect")
# cate_crp_sb <- predict(fit_crp_sb, newdata = X_hte, type = "treatment.effect")
# cate_sb_crp <- predict(fit_sb_crp, newdata = X_hte, type = "treatment.effect")
# cate_sb_sb <- predict(fit_sb_sb, newdata = X_hte, type = "treatment.effect")
# 
# df_hte <- data.frame(
#   x1 = X_hte[, 1],
#   `CRP+CRP` = cate_crp_crp,
#   `CRP+SB` = cate_crp_sb,
#   `SB+CRP` = cate_sb_crp,
#   `SB+SB` = cate_sb_sb,
#   check.names = FALSE
# )
# 
# # Melt for ggplot
# df_hte_long <- reshape2::melt(df_hte, id.vars = "x1",
#                                variable.name = "Combo", value.name = "CATE")
# 
# p_hte <- ggplot(df_hte_long, aes(x = x1, y = CATE, color = Combo)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   labs(title = "CATE: All Backend Combinations", x = "X1", y = "CATE") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# print(p_hte)

## ----ps-variants--------------------------------------------------------------
# # SB PS + CRP Outcome with Probit PS
# bundle_probit <- build_causal_bundle(
#   y = y, T = T, X = X,
#   kernel = "normal", backend = "crp",
#   PS = "probit",           # Probit instead of logit
#   GPD = FALSE, Kmax = 10,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# fit_probit <- run_mcmc_bundle_manual(bundle_probit)
# 
# ate_probit <- coef(fit_probit)
# cat("\n=== PS MODEL VARIANT COMPARISON ===\n")
# cat("Logit PS + CRP Outcome ATE: ", ate_sb_crp, "\n")
# cat("Probit PS + CRP Outcome ATE:", ate_probit, "\n")
# cat("Difference:", abs(ate_sb_crp - ate_probit), "\n")

## ----efficiency---------------------------------------------------------------
# cat("\n=== COMPUTATIONAL TRADE-OFFS ===\n\n")
# 
# cat("CRP + CRP:\n")
# cat("  Pros: Most flexible both PS and outcome\n")
# cat("  Cons: Slowest\n\n")
# 
# cat("CRP + SB:\n")
# cat("  Pros: Flexible PS, efficient outcome\n")
# cat("  Cons: Medium speed\n\n")
# 
# cat("SB + CRP:\n")
# cat("  Pros: Efficient PS, flexible outcome\n")
# cat("  Cons: Medium speed\n\n")
# 
# cat("SB + SB:\n")
# cat("  Pros: Fastest, simplest\n")
# cat("  Cons: Least flexible\n")

## ----recommendation-----------------------------------------------------------
# cat("\n=== CHOOSING BACKENDS ===\n\n")
# 
# cat("Step 1: Check data complexity\n")
# cat("  - Sample size?\n")
# cat("  - Treatment assignment complexity?\n")
# cat("  - Outcome distribution shape?\n\n")
# 
# cat("Step 2: Fit models with different backends\n")
# cat("  - Check convergence (Rhat, ESS)\n")
# cat("  - Compare treatment effects\n\n")
# 
# cat("Step 3: Select based on:\n")
# cat("  ✓ Computational budget\n")
# cat("  ✓ Convergence diagnostics\n")
# cat("  ✓ Stability across runs\n")

## ----summary------------------------------------------------------------------
# cat("\n=== 15-VIGNETTE ROADMAP ===\n\n")
# 
# cat("v01: Introduction - Overview & three-phase workflow\n")
# cat("v02: Distributions - All 7 kernels + GPD\n")
# cat("v03: Basic workflow - Spec → Bundle → MCMC\n\n")
# 
# cat("v04-v05: Unconditional DPmix\n")
# cat("  v04: CRP backend\n")
# cat("  v05: SB backend\n\n")
# 
# cat("v06-v07: Unconditional DPmixGPD\n")
# cat("  v06: CRP + GPD tail\n")
# cat("  v07: SB + GPD tail\n\n")
# 
# cat("v08-v09: Conditional DPmix\n")
# cat("  v08: CRP + covariates\n")
# cat("  v09: SB + covariates\n\n")
# 
# cat("v10-v11: Conditional DPmixGPD\n")
# cat("  v10: CRP + covariates + GPD tail\n")
# cat("  v11: SB + covariates + GPD tail\n\n")
# 
# cat("v12-v13: Causal Same Backend\n")
# cat("  v12: CRP for both PS & outcome\n")
# cat("  v13: SB for both PS & outcome\n\n")
# 
# cat("v14-v15: Causal Different Backends\n")
# cat("  v14: CRP PS, varied outcome\n")
# cat("  v15: SB PS, varied outcome (YOU ARE HERE)\n\n")
# 
# cat("✓ COMPLETE 15-VIGNETTE TUTORIAL SERIES\n")

