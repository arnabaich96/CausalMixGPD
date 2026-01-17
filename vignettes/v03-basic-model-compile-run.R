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
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)
library(ggplot2)
set.seed(42)

## ----spec-basic---------------------------------------------------------------
# # Load packaged data
# data("nc_pos200_k3")
# y <- nc_pos200_k3$y
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
# print("Specification object class:", class(spec), "\n")
# print("Kernel chosen:", spec$kernel, "\n")
# print("Backend chosen:", spec$backend, "\n")
# print("GPD enabled:", spec$GPD, "\n")
# print("Sample size n:", length(spec$y), "\n")

## ----spec-variants------------------------------------------------------------
# # 1. Unconditional DPmix (bulk only, no tail)
# spec_unconditional <- compile_model_spec(y, kernel = "gamma", backend = "crp", GPD = FALSE)
# 
# # 2. Unconditional DPmixGPD (bulk + tail)
# spec_gpd <- compile_model_spec(y, kernel = "gamma", backend = "crp", GPD = TRUE)
# 
# # 3. Conditional (with covariates)
# data("nc_posX100_p3_k2")
# y_cond <- nc_posX100_p3_k2$y
# X <- as.matrix(nc_posX100_p3_k2$X)
# spec_conditional <- compile_model_spec(
#   y = y_cond,
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
#   mcmc = list(niter = 1500, nburnin = 250, nchains = 2, thin = 2)
# )
# 
# print("Created 4 specification objects successfully.\n")

## ----bundle-from-spec---------------------------------------------------------
# # Method 1: From specification object
# bundle <- build_nimble_bundle(spec)
# 
# print("Bundle object class:", class(bundle), "\n")
# print("Bundle contains compiled sampler ready for MCMC.\n")

## ----bundle-direct------------------------------------------------------------
# # Method 2: Direct call (recommended for quick workflows)
# bundle_direct <- build_nimble_bundle(
#   y = y,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE,
#   mcmc = list(niter = 1500, nburnin = 250, nchains = 2)
# )
# 
# print("Direct bundle creation successful.\n")

## ----bundle-inspect-----------------------------------------------------------
# # Bundle is an S3 object with structure
# print("Bundle class:", class(bundle_direct), "\n")
# print("Bundle contains:\n")
# print(names(bundle_direct))
# 
# # Access key components
# print("\nMCMC settings:\n")
# print(bundle_direct$mcmc_settings)

## ----mcmc-basic---------------------------------------------------------------
# # Run MCMC
# fit <- run_mcmc_bundle_manual(bundle_direct)
# 
# print("Fit object class:", class(fit), "\n")
# print("MCMC execution complete. Posterior samples collected.\n")

## ----mcmc-posterior-----------------------------------------------------------
# # Posterior summary
# print("\n--- POSTERIOR SUMMARY ---\n")
# summary(fit)
# 
# # Posterior mean parameters in original form
# params_fit <- params(fit)
# params_fit

## ----mcmc-diagnostics---------------------------------------------------------
# # Trace plots
# plot(fit, params = "alpha|beta", family = c("traceplot", "running", "autocorrelation"))

## ----workflow-complete--------------------------------------------------------
# # Load packaged data
# data("nc_pos200_k3")
# y_data <- nc_pos200_k3$y
# 
# # PHASE 1: Specification
# spec_final <- compile_model_spec(
#   y = y_data,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = FALSE,
#   Kmax = 5
# )
# 
# # PHASE 2: Bundle
# bundle_final <- build_nimble_bundle(
#   spec_final,
#   mcmc = list(
#     niter = 60,
#     nburnin = 10,
#     nchains = 2,
#     thin = 1
#   )
# )
# 
# # PHASE 3: MCMC
# fit_final <- run_mcmc_bundle_manual(bundle_final)
# 
# print("\n=== THREE-PHASE WORKFLOW COMPLETE ===\n")
# summary(fit_final)

## ----backend-crp--------------------------------------------------------------
# # Chinese Restaurant Process
# bundle_crp <- build_nimble_bundle(
#   y = y_data,
#   kernel = "gamma",
#   backend = "crp",
#   Kmax = 5,
#   mcmc = list(niter = 1500, nburnin = 250, nchains = 2)
# )
# 
# fit_crp <- run_mcmc_bundle_manual(bundle_crp)
# print("CRP execution complete.\n")

## ----backend-sb---------------------------------------------------------------
# # Stick-Breaking Process
# bundle_sb <- build_nimble_bundle(
#   y = y_data,
#   kernel = "gamma",
#   backend = "sb",
#   J = 5,
#   mcmc = list(niter = 100, nburnin = 20, nchains = 3)
# )
# 
# fit_sb <- run_mcmc_bundle_manual(bundle_sb)
# print("SB execution complete.\n")

## ----kernel-guide-------------------------------------------------------------
# kernels_available <- c("gamma", "lognormal", "normal", "laplace", "invgauss", "amoroso", "cauchy")
# 
# print("Available kernels:\n")
# for (k in kernels_available) {
#   print("  -", k, "\n")
# }
# 
# print("\nChoose kernel based on:\n")
# print("  gamma:     Right-skewed, positive support\n")
# print("  lognormal: Log-transformed normality\n")
# print("  normal:    Symmetric, unbounded\n")
# print("  laplace:   Sharp peak, exponential tails\n")
# print("  invgauss:  Positive, near-normal shape\n")
# print("  amoroso:   Generalized, maximum flexibility\n")
# print("  cauchy:    Heavy-tailed, rare cases\n")

## ----gpd-example--------------------------------------------------------------
# # Data with tail behavior
# data("nc_pos_tail200_k4")
# y_tail <- nc_pos_tail200_k4$y
# 
# # Build with GPD
# bundle_gpd <- build_nimble_bundle(
#   y = y_tail,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = TRUE,
#   mcmc = list(niter = 2500, nburnin = 500, nchains = 1)
# )
# 
# fit_gpd <- run_mcmc_bundle_manual(bundle_gpd)
# print("\nGPD augmentation applied to tail region.\n")
# summary(fit_gpd)

## ----param-reference----------------------------------------------------------
# print("=== Recommended MCMC Parameters ===\n")
# print("Quick test:     niter=500,  nburnin=100, nchains=1\n")
# print("Standard:       niter=1000, nburnin=250, nchains=2\n")
# print("Production:     niter=1000, nburnin=250, nchains=3\n")
# print("\n=== Backend Parameters ===\n")
# print("CRP: Kmax=3-5 (truncation for # components)\n")
# print("SB:  J=3-5    (fixed # components)\n")
# print("\n=== Kernel Selection ===\n")
# print("Positive data:     gamma, lognormal, invgauss\n")
# print("Any real data:     normal\n")
# print("Symmetric tails:   laplace\n")
# print("Extreme outliers:  cauchy\n")

