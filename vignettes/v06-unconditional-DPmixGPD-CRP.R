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
# # Generate bimodal bulk + heavy tail
# set.seed(42)
# bulk_comp1 <- rgamma(80, shape = 2, rate = 1.5)
# bulk_comp2 <- rgamma(60, shape = 1, rate = 0.8)
# bulk <- c(bulk_comp1, bulk_comp2)
# 
# # Add extreme tail (GPD)
# tail_component <- DPmixGPD::rGpd(30, threshold = 0, scale = 1.5, shape = 0.3)
# 
# y_tail <- c(bulk, tail_component)
# 
# cat("Sample size:", length(y_tail), "\n")
# cat("Bulk observations:", length(bulk), "\n")
# cat("Tail observations:", length(tail_component), "\n")
# cat("Min:", min(y_tail), "\n")
# cat("Max:", max(y_tail), "\n")
# cat("Mean:", mean(y_tail), "\n")
# 
# # Visualization
# df_raw <- data.frame(y = y_tail, region = ifelse(y_tail > quantile(y_tail, 0.8), "Tail", "Bulk"))
# 
# p_raw <- ggplot(df_raw, aes(x = y, fill = region)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.7) +
#   geom_density(alpha = 0.3, linewidth = 1) +
#   scale_fill_manual(values = c("Bulk" = "steelblue", "Tail" = "red")) +
#   labs(title = "Data with Bulk & Tail Regions", x = "y", y = "Density") +
#   theme_minimal()
# 
# print(p_raw)

## ----threshold-selection------------------------------------------------------
# # Options for threshold selection:
# threshold_mean <- mean(y_tail)
# threshold_quantile90 <- quantile(y_tail, 0.9)
# threshold_quantile95 <- quantile(y_tail, 0.95)
# 
# cat("Threshold Options:\n")
# cat("  Mean:           ", round(threshold_mean, 3), "\n")
# cat("  90th percentile:", round(threshold_quantile90, 3), "\n")
# cat("  95th percentile:", round(threshold_quantile95, 3), "\n\n")
# 
# # Use 80th percentile as default (typically ~20% in tail)
# u_threshold <- quantile(y_tail, 0.8)
# cat("Selected threshold (80th percentile):", round(u_threshold, 3), "\n")

## ----spec-gpd-crp-------------------------------------------------------------
# spec_gpd <- compile_model_spec(
#   y = y_tail,
#   kernel = "gamma",           # Bulk kernel
#   backend = "crp",            # CRP for bulk
#   GPD = TRUE,                 # ENABLE tail augmentation
#   threshold = u_threshold,    # Tail threshold
#   Kmax = 15,                  # Max bulk components
#   verbose = FALSE
# )
# 
# cat("Specification with GPD:\n")
# cat("  Kernel (bulk):", spec_gpd$kernel, "\n")
# cat("  Backend (bulk):", spec_gpd$backend, "\n")
# cat("  Threshold:", u_threshold, "\n")
# cat("  GPD enabled:", spec_gpd$GPD, "\n")
# cat("  Bulk observations:", sum(y_tail <= u_threshold), "\n")
# cat("  Tail observations:", sum(y_tail > u_threshold), "\n")

## ----bundle-mcmc-gpd----------------------------------------------------------
# bundle_gpd <- build_nimble_bundle(
#   spec_gpd,
#   mcmc = list(
#     niter = 2000,
#     nburnin = 500,
#     nchains = 2,
#     thin = 1
#   )
# )
# 
# cat("Bundle compiled with CRP (bulk) + GPD (tail).\n")

## ----mcmc-gpd-----------------------------------------------------------------
# fit_gpd <- run_mcmc_bundle_manual(bundle_gpd)
# 
# cat("\n=== POSTERIOR SUMMARY (DPmixGPD, CRP Backend) ===\n")
# summary(fit_gpd)

## ----gpd-params---------------------------------------------------------------
# cat("Tail Parameters Estimated:\n")
# cat("  scale (σ):   > 0, controls tail spread\n")
# cat("  shape (ξ):   > 0, controls tail heaviness\n")
# cat("             ξ = 0 (exponential tail)\n")
# cat("             ξ > 0 (power-law tail)\n")
# cat("             ξ < 0 (finite tail)\n\n")
# 
# cat("Interpret from posterior summary above:\n")
# cat("Look for 'scale' and 'shape' parameters in MCMC output.\n")

## ----pred-density-------------------------------------------------------------
# # Full prediction grid including tail
# y_grid <- seq(0, max(y_tail) * 1.3, length.out = 300)
# 
# pred_densities <- predict(fit_gpd, newdata = y_grid)
# 
# df_pred <- data.frame(
#   y = y_grid,
#   density = pred_densities,
#   region = ifelse(y_grid <= u_threshold, "Bulk", "Tail")
# )
# 
# p_pred <- ggplot(df_raw, aes(x = y)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.4, fill = "steelblue") +
#   geom_line(data = df_pred, aes(x = y, y = density, color = region), linewidth = 1.2) +
#   geom_vline(xintercept = u_threshold, linetype = "dashed", color = "gray") +
#   scale_color_manual(values = c("Bulk" = "steelblue", "Tail" = "red")) +
#   labs(title = "Posterior Predictive with Bulk & Tail", x = "y", y = "Density") +
#   theme_minimal()
# 
# print(p_pred)

## ----pred-tail-zoom-----------------------------------------------------------
# # Zoom on tail
# tail_idx <- df_pred$y > u_threshold
# 
# p_tail <- ggplot(df_pred[tail_idx, ], aes(x = y, y = density)) +
#   geom_line(color = "red", linewidth = 1.2) +
#   geom_area(alpha = 0.3, fill = "red") +
#   labs(title = "GPD Tail (Posterior Predictive)", x = "y", y = "Density") +
#   theme_minimal()
# 
# print(p_tail)

## ----tail-prob----------------------------------------------------------------
# # P(Y > threshold) - probability in tail
# tail_exceedance <- mean(y_tail > u_threshold)
# 
# cat("Empirical tail exceedance:", tail_exceedance, "\n")
# 
# # Posterior predictive P(Y > y) for high values
# high_y_values <- seq(u_threshold, max(y_tail) * 1.1, length.out = 20)
# exceedance_probs <- 1 - predict(fit_gpd, newdata = high_y_values, type = "cdf")
# 
# df_exc <- data.frame(
#   y = high_y_values,
#   prob_exceed = exceedance_probs
# )
# 
# p_exc <- ggplot(df_exc, aes(x = y, y = prob_exceed)) +
#   geom_line(color = "darkred", linewidth = 1.2) +
#   geom_point(color = "darkred", size = 2) +
#   labs(title = "P(Y > y | Data): Tail Exceedance", x = "y", y = "Probability") +
#   theme_minimal()
# 
# print(p_exc)

## ----comp-no-gpd--------------------------------------------------------------
# bundle_bulk_only <- build_nimble_bundle(
#   y = y_tail,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE,
#   Kmax = 15,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# fit_bulk_only <- run_mcmc_bundle_manual(bundle_bulk_only)
# # Model fit completed

## ----comp-with-gpd------------------------------------------------------------
# # Model fit completed

## ----comp-summary-------------------------------------------------------------
# cat("\n=== BULK ONLY vs BULK + GPD ===\n")
# cat("Interpretation:\n")
# if (loglik_gpd > loglik_bulk) {
#   cat("✓ GPD significantly improves fit (tail is important)\n")
# } else {
#   cat("✗ GPD doesn't help (no strong tail behavior)\n")
# }

## ----return-levels------------------------------------------------------------
# # Estimate rare event probabilities
# probs <- c(0.99, 0.95, 0.90)
# return_levels <- predict(fit_gpd, newdata = data.frame(p = probs), type = "quantile")
# 
# cat("Return Levels (Posterior Mean):\n")
# cat("  99th percentile:", return_levels[1], "\n")
# cat("  95th percentile:", return_levels[2], "\n")
# cat("  90th percentile:", return_levels[3], "\n")

## ----residuals----------------------------------------------------------------
# residuals_vals <- residuals(fit_gpd)
# 
# df_resid <- data.frame(residuals = residuals_vals)
# 
# p_resid <- ggplot(df_resid, aes(x = residuals)) +
#   geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.6, fill = "mediumseagreen") +
#   geom_density(color = "darkgreen", linewidth = 1) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
#   labs(title = "Residuals: DPmixGPD (CRP)", x = "Residuals", y = "Density") +
#   theme_minimal()
# 
# print(p_resid)
# 
# cat("Residual Summary:\n")
# print(summary(residuals_vals))

## ----threshold-sensitivity----------------------------------------------------
# thresholds <- quantile(y_tail, c(0.70, 0.75, 0.80, 0.85, 0.90))
# 
# cat("Testing threshold percentiles:\n")
# cat("Values:", paste(names(thresholds), collapse = ", "), "\n")
# cat("Recommendation: Use 0.80-0.90 for heavy tails.\n")
# cat("Compare convergence diagnostics via summary().\n")
# 
# # Demonstrate with one threshold
# bundle_thresh <- build_nimble_bundle(
#   y = y_tail,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = TRUE,
#   threshold = quantile(y_tail, 0.85),
#   Kmax = 15,
#   mcmc = list(niter = 500, nburnin = 100, nchains = 1)
# )
# fit_thresh <- run_mcmc_bundle_manual(bundle_thresh)
# summary(fit_thresh)

