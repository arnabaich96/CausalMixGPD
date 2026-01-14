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
set.seed(42)

## ----spec-basic---------------------------------------------------------------
# # Simulate data
# set.seed(123)
# y <- rgamma(100, shape = 2, rate = 1)
# 
# # Create specification
# spec <- compile_model_spec(
#   y = y,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE
# )
# 
# # Inspect specification
# cat("Specification object class:", class(spec), "\n")
# cat("Kernel chosen:", spec$kernel, "\n")
# cat("Backend chosen:", spec$backend, "\n")
# cat("GPD enabled:", spec$GPD, "\n")
# cat("Sample size n:", length(spec$y), "\n")

## ----spec-variants------------------------------------------------------------
# # 1. Unconditional DPmix (bulk only, no tail)
# spec_unconditional <- compile_model_spec(y, kernel = "gamma", backend = "crp", GPD = FALSE)
# 
# # 2. Unconditional DPmixGPD (bulk + tail)
# spec_gpd <- compile_model_spec(y, kernel = "gamma", backend = "crp", GPD = TRUE)
# 
# # 3. Conditional (with covariates)
# X <- matrix(rnorm(100 * 2), nrow = 100, ncol = 2)
# spec_conditional <- compile_model_spec(
#   y = y,
#   X = X,
#   kernel = "lognormal",
#   backend = "crp",
#   GPD = FALSE
# )
# 
# # 4. With custom MCMC settings
# spec_custom <- compile_model_spec(
#   y = y,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE,
#   mcmc = list(niter = 2000, nburnin = 500, nchains = 2, thin = 2)
# )
# 
# cat("Created 4 specification objects successfully.\n")

## ----bundle-from-spec---------------------------------------------------------
# # Method 1: From specification object
# bundle <- build_nimble_bundle(spec)
# 
# cat("Bundle object class:", class(bundle), "\n")
# cat("Bundle contains compiled sampler ready for MCMC.\n")

## ----bundle-direct------------------------------------------------------------
# # Method 2: Direct call (recommended for quick workflows)
# bundle_direct <- build_nimble_bundle(
#   y = y,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE,
#   mcmc = list(niter = 500, nburnin = 100, nchains = 1)
# )
# 
# cat("Direct bundle creation successful.\n")

## ----bundle-inspect-----------------------------------------------------------
# # Bundle is an S3 object with structure
# cat("Bundle class:", class(bundle_direct), "\n")
# cat("Bundle contains:\n")
# print(names(bundle_direct))
# 
# # Access key components
# cat("\nMCMC settings:\n")
# print(bundle_direct$mcmc_settings)

## ----mcmc-basic---------------------------------------------------------------
# # Run MCMC
# fit <- run_mcmc_bundle_manual(bundle_direct)
# 
# cat("Fit object class:", class(fit), "\n")
# cat("MCMC execution complete. Posterior samples collected.\n")

## ----mcmc-posterior-----------------------------------------------------------
# # Posterior summary
# cat("\n--- POSTERIOR SUMMARY ---\n")
# summary(fit)

## ----mcmc-diagnostics---------------------------------------------------------
# # Trace plots
# plot(fit, params = "alpha|beta")

## ----workflow-complete--------------------------------------------------------
# # Generate realistic data
# set.seed(789)
# y_data <- rgamma(150, shape = 1.8, rate = 0.9)
# 
# # PHASE 1: Specification
# spec_final <- compile_model_spec(
#   y = y_data,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE,
#   Kmax = 15
# )
# 
# # PHASE 2: Bundle
# bundle_final <- build_nimble_bundle(
#   spec_final,
#   mcmc = list(
#     niter = 1000,
#     nburnin = 200,
#     nchains = 2,
#     thin = 1
#   )
# )
# 
# # PHASE 3: MCMC
# fit_final <- run_mcmc_bundle_manual(bundle_final)
# 
# cat("\n=== THREE-PHASE WORKFLOW COMPLETE ===\n")
# summary(fit_final)

## ----backend-crp--------------------------------------------------------------
# # Chinese Restaurant Process
# bundle_crp <- build_nimble_bundle(
#   y = y_data,
#   kernel = "gamma",
#   backend = "crp",
#   Kmax = 10,
#   mcmc = list(niter = 500, nburnin = 100, nchains = 1)
# )
# 
# fit_crp <- run_mcmc_bundle_manual(bundle_crp)
# cat("CRP execution complete.\n")

## ----backend-sb---------------------------------------------------------------
# # Stick-Breaking Process
# bundle_sb <- build_nimble_bundle(
#   y = y_data,
#   kernel = "gamma",
#   backend = "sb",
#   J = 10,
#   mcmc = list(niter = 500, nburnin = 100, nchains = 1)
# )
# 
# fit_sb <- run_mcmc_bundle_manual(bundle_sb)
# cat("SB execution complete.\n")

## ----kernel-guide-------------------------------------------------------------
# kernels_available <- c("gamma", "lognormal", "normal", "laplace", "invgauss", "amoroso", "cauchy")
# 
# cat("Available kernels:\n")
# for (k in kernels_available) {
#   cat("  -", k, "\n")
# }
# 
# cat("\nChoose kernel based on:\n")
# cat("  gamma:     Right-skewed, positive support\n")
# cat("  lognormal: Log-transformed normality\n")
# cat("  normal:    Symmetric, unbounded\n")
# cat("  laplace:   Sharp peak, exponential tails\n")
# cat("  invgauss:  Positive, near-normal shape\n")
# cat("  amoroso:   Generalized, maximum flexibility\n")
# cat("  cauchy:    Heavy-tailed, rare cases\n")

## ----gpd-example--------------------------------------------------------------
# # Data with tail behavior
# y_tail <- c(rgamma(120, shape = 2, rate = 1), rgamma(30, shape = 0.8, rate = 0.1))
# 
# # Build with GPD
# bundle_gpd <- build_nimble_bundle(
#   y = y_tail,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = TRUE,
#   mcmc = list(niter = 500, nburnin = 100, nchains = 1)
# )
# 
# fit_gpd <- run_mcmc_bundle_manual(bundle_gpd)
# cat("\nGPD augmentation applied to tail region.\n")
# summary(fit_gpd)

## ----param-reference----------------------------------------------------------
# cat("=== Recommended MCMC Parameters ===\n")
# cat("Quick test:     niter=500,  nburnin=100, nchains=1\n")
# cat("Standard:       niter=2000, nburnin=500, nchains=2\n")
# cat("Production:     niter=5000, nburnin=1000, nchains=3\n")
# cat("\n=== Backend Parameters ===\n")
# cat("CRP: Kmax=10-30 (truncation for # components)\n")
# cat("SB:  J=10-30    (fixed # components)\n")
# cat("\n=== Kernel Selection ===\n")
# cat("Positive data:     gamma, lognormal, invgauss\n")
# cat("Any real data:     normal\n")
# cat("Symmetric tails:   laplace\n")
# cat("Extreme outliers:  cauchy\n")

