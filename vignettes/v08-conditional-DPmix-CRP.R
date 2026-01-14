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
# n <- 150
# 
# # Covariates
# X <- cbind(
#   x1 = rnorm(n, mean = 0, sd = 1),
#   x2 = rnorm(n, mean = 0, sd = 1)
# )
# 
# # Outcome: mixture conditional on X
# mean_shift_1 <- 1 + 0.5 * X[, 1] + 0.3 * X[, 2]
# mean_shift_2 <- 3 + 0.2 * X[, 1] - 0.4 * X[, 2]
# 
# component_1 <- rgamma(75, shape = 2, rate = 1.5) + mean_shift_1[1:75]
# component_2 <- rgamma(75, shape = 1, rate = 0.8) + mean_shift_2[76:150]
# y <- c(component_1, component_2)
# 
# cat("Sample size:", n, "\n")
# cat("Covariates:", ncol(X), "\n")
# 
# # Visualization
# df_data <- data.frame(y = y, x1 = X[, 1], x2 = X[, 2])
# 
# p_scatter <- ggplot(df_data, aes(x = x1, y = y)) +
#   geom_point(alpha = 0.6, color = "steelblue") +
#   geom_smooth(method = "loess", color = "red", fill = NA) +
#   labs(title = "Outcome vs Covariate X1", x = "X1", y = "y") +
#   theme_minimal()
# 
# print(p_scatter)

## ----spec-conditional---------------------------------------------------------
# spec_cond <- compile_model_spec(
#   y = y,
#   X = X,                     # NEW: Include covariates
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE,
#   Kmax = 15,
#   verbose = FALSE
# )
# 
# cat("Conditional Specification:\n")
# cat("  Outcome (y):", length(y), "observations\n")
# cat("  Covariates (X):", nrow(X), "×", ncol(X), "\n")
# cat("  Kernel: gamma (conditional)\n")
# cat("  Backend: CRP\n")

## ----bundle-mcmc--------------------------------------------------------------
# bundle_cond <- build_nimble_bundle(
#   spec_cond,
#   mcmc = list(
#     niter = 2000,
#     nburnin = 500,
#     nchains = 2,
#     thin = 1
#   )
# )
# 
# fit_cond <- run_mcmc_bundle_manual(bundle_cond)
# 
# cat("\n=== POSTERIOR SUMMARY (Conditional, CRP) ===\n")
# summary(fit_cond)

## ----pred-conditional---------------------------------------------------------
# # New covariate values for prediction
# X_new <- cbind(
#   x1 = c(-1, 0, 1),
#   x2 = c(-1, 0, 1)
# )
# 
# # Predict for each covariate combination
# y_grid <- seq(-2, 10, length.out = 100)
# 
# # For each row of X_new, get conditional density
# densities_list <- list()
# for (i in 1:nrow(X_new)) {
#   X_rep <- X_new[i, , drop = FALSE]
#   dens <- predict(fit_cond, newdata = y_grid, X = X_rep)
#   densities_list[[i]] <- data.frame(
#     y = y_grid,
#     density = dens,
#     group = paste("X1=", X_new[i, 1], ", X2=", X_new[i, 2], sep = "")
#   )
# }
# 
# df_cond_pred <- do.call(rbind, densities_list)
# 
# p_cond_pred <- ggplot(df_cond_pred, aes(x = y, y = density, color = group)) +
#   geom_line(linewidth = 1) +
#   labs(title = "Conditional Predictive Density", x = "y", y = "Density") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# print(p_cond_pred)

## ----covariate-effect---------------------------------------------------------
# # Isolate effect of X1 (holding X2 = 0)
# X1_range <- seq(-2, 2, by = 0.5)
# X2_fixed <- 0
# 
# for (x1_val in X1_range[c(1, 3, 5)]) {  # Show 3 examples
#   X_eval <- cbind(x1 = x1_val, x2 = X2_fixed)
#   y_grid <- seq(-1, 10, length.out = 100)
#   dens <- predict(fit_cond, newdata = y_grid, X = X_eval)
# 
#   cat("X1 =", x1_val, "=> Posterior predictive mean:", weighted.mean(y_grid, dens), "\n")
# }

## ----covariate-coef-----------------------------------------------------------
# # Extract covariate effects from posterior
# coefs <- coef(fit_cond)
# cat("Covariate coefficients (posterior mean):\n")
# print(head(coefs))

## ----heteroscedasticity-------------------------------------------------------
# # Fitted values and residuals by X1 quintile
# fitted_vals <- fitted(fit_cond)
# residuals_vals <- residuals(fit_cond)
# 
# X1_quintile <- cut(X[, 1], breaks = 5, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
# 
# df_het <- data.frame(
#   residual = residuals_vals,
#   quintile = X1_quintile,
#   fitted = fitted_vals
# )
# 
# p_het <- ggplot(df_het, aes(x = quintile, y = abs(residual))) +
#   geom_boxplot(alpha = 0.6, fill = "steelblue") +
#   geom_jitter(alpha = 0.3, width = 0.2) +
#   labs(title = "Absolute Residuals by X1 Quintile", x = "X1 Quintile", y = "|Residual|") +
#   theme_minimal()
# 
# print(p_het)

## ----comparison---------------------------------------------------------------
# # Fit unconditional model on same data
# bundle_uncond <- build_nimble_bundle(
#   y = y,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE,
#   Kmax = 15,
#   mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
# )
# fit_uncond <- run_mcmc_bundle_manual(bundle_uncond)
# 
# cat("\n=== MODEL COMPARISON ===\n")

