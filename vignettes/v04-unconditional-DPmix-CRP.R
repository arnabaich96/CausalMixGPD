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
library(gridExtra)
set.seed(123)

## ----data-setup---------------------------------------------------------------
# # Generate from mixture of gamma distributions
# set.seed(42)
# component1 <- rgamma(60, shape = 2, rate = 1.5)
# component2 <- rgamma(40, shape = 1, rate = 0.5)
# y_mixed <- c(component1, component2)
# 
# cat("Sample size:", length(y_mixed), "\n")
# cat("Mean:", mean(y_mixed), "\n")
# cat("SD:", sd(y_mixed), "\n")
# cat("Range:", range(y_mixed), "\n")
# 
# # Visualization
# df_data <- data.frame(y = y_mixed)
# p_raw <- ggplot(df_data, aes(x = y)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6, fill = "steelblue") +
#   geom_density(color = "red", linewidth = 1) +
#   labs(title = "Raw Data: Mixed Gamma Distribution", x = "y", y = "Density") +
#   theme_minimal()
# 
# print(p_raw)

## ----spec-crp-----------------------------------------------------------------
# spec_crp <- compile_model_spec(
#   y = y_mixed,
#   kernel = "gamma",           # Use gamma kernel
#   backend = "crp",            # CRP backend
#   GPD = FALSE,                # No tail augmentation
#   Kmax = 15,                  # Max components truncation
#   alpha = 1.0,                # DP concentration
#   verbose = FALSE
# )
# 
# cat("Specification created:\n")
# cat("  Kernel:", spec_crp$kernel, "\n")
# cat("  Backend:", spec_crp$backend, "\n")
# cat("  Kmax:", spec_crp$Kmax, "\n")
# cat("  GPD:", spec_crp$GPD, "\n")

## ----bundle-mcmc-crp----------------------------------------------------------
# bundle_crp <- build_nimble_bundle(
#   spec_crp,
#   mcmc = list(
#     niter = 2000,           # Total iterations
#     nburnin = 500,          # Burn-in
#     nchains = 2,            # Parallel chains
#     thin = 1                # No thinning
#   )
# )
# 
# cat("Bundle compiled with CRP sampler.\n")
# cat("Ready for MCMC execution.\n")

## ----mcmc-crp-----------------------------------------------------------------
# fit_crp <- run_mcmc_bundle_manual(bundle_crp)
# 
# cat("\n=== POSTERIOR SUMMARY (CRP Backend) ===\n")
# summary(fit_crp)

## ----diag-trace---------------------------------------------------------------
# # Trace plots for key parameters
# plot(fit_crp, params = "alpha|beta")

## ----diag-ess-----------------------------------------------------------------
# # ESS and R-hat from summary
# cat("Check ESS > 1000 and Rhat < 1.1 for convergence.\n")

## ----pred-density-------------------------------------------------------------
# # Generate prediction grid
# y_grid <- seq(0, max(y_mixed) * 1.2, length.out = 200)
# 
# # Posterior predictive density
# pred_densities <- predict(fit_crp, newdata = y_grid)
# 
# df_pred <- data.frame(
#   y = y_grid,
#   density = pred_densities
# )
# 
# # Combine with data
# p_pred <- ggplot(df_data, aes(x = y)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.4, fill = "steelblue") +
#   geom_line(data = df_pred, aes(x = y, y = density), color = "red", linewidth = 1.2) +
#   labs(title = "Data vs Posterior Predictive Density (CRP)",
#        x = "y", y = "Density") +
#   theme_minimal()
# 
# print(p_pred)

## ----pred-quantiles-----------------------------------------------------------
# # Posterior predictive quantiles
# quantiles_pred <- predict(fit_crp, newdata = y_grid, type = "quantile",
#                           probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
# 
# cat("Posterior Predictive Quantiles:\n")
# print(head(quantiles_pred, 10))

## ----comp-analysis------------------------------------------------------------
# # Extract number of components used
# # (CRP backend infers K from cluster assignments)
# cat("The CRP allows flexibility in # components.\n")
# cat("Check MCMC samples for K (number of occupied tables).\n")

## ----comp-params--------------------------------------------------------------
# # Estimated component parameters
# # E.g., for gamma kernel: shape and rate per component
# cat("Posterior mean component parameters available via coef().\n")
# coef_vals <- coef(fit_crp)
# print(head(coef_vals))

## ----comp-gamma---------------------------------------------------------------
# bundle_gamma <- build_nimble_bundle(
#   y = y_mixed,
#   kernel = "gamma",
#   backend = "crp",
#   Kmax = 15,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# fit_gamma <- run_mcmc_bundle_manual(bundle_gamma)
# summary(fit_gamma)

## ----comp-lognormal-----------------------------------------------------------
# bundle_lognormal <- build_nimble_bundle(
#   y = y_mixed,
#   kernel = "lognormal",
#   backend = "crp",
#   Kmax = 15,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# fit_lognormal <- run_mcmc_bundle_manual(bundle_lognormal)
# summary(fit_lognormal)

## ----comp-summary-------------------------------------------------------------
# # Compare fitted values
# fitted_gamma <- fitted(fit_gamma)
# fitted_lognormal <- fitted(fit_lognormal)
# 
# cat("\n=== KERNEL COMPARISON (CRP Backend) ===\n")
# cat("Gamma fitted values (first 5):    ", head(fitted_gamma, 5), "\n")
# cat("Lognormal fitted values (first 5):", head(fitted_lognormal, 5), "\n")
# cat("\nUse summary() to compare convergence diagnostics.\n")

## ----kmax-sensitivity---------------------------------------------------------
# kmax_values <- c(5, 10, 15, 20)
# 
# cat("Testing Kmax values:", paste(kmax_values, collapse = ", "), "\n")
# cat("Recommendation: Start with Kmax=10-15 for most applications.\n")
# cat("Monitor effective number of components via summary().\n")
# 
# # Demonstrate with one value
# bundle_kmax <- build_nimble_bundle(
#   y = y_mixed,
#   kernel = "gamma",
#   backend = "crp",
#   Kmax = 10,
#   mcmc = list(niter = 500, nburnin = 100, nchains = 1)
# )
# fit_kmax <- run_mcmc_bundle_manual(bundle_kmax)
# summary(fit_kmax)

## ----residuals----------------------------------------------------------------
# # Extract residuals
# residuals_vals <- residuals(fit_crp)
# 
# df_resid <- data.frame(residuals = residuals_vals)
# 
# p_resid <- ggplot(df_resid, aes(x = residuals)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6, fill = "coral") +
#   geom_density(color = "darkred", linewidth = 1) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
#   labs(title = "Residual Distribution", x = "Residuals", y = "Density") +
#   theme_minimal()
# 
# print(p_resid)
# 
# cat("Residual Summary:\n")
# print(summary(residuals_vals))

