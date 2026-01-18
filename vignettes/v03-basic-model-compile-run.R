## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 8,
  fig.height = 6,
  message = FALSE,
  warning = FALSE,
  eval = TRUE,
  cache = TRUE,
  cache.path = "../.cache/vignettes/"
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)
library(ggplot2)
set.seed(42)

## ----bundle-direct------------------------------------------------------------
# Load packaged data
data("nc_pos200_k3")
y <- nc_pos200_k3$y

# Direct call
bundle_direct <- build_nimble_bundle(
  y = y,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  components = 5,
  mcmc = list(niter = 200, nburnin = 50, nchains = 1)
)

print("Direct bundle creation successful.\n")

## ----bundle-inspect-----------------------------------------------------------
# Bundle is an S3 object with structure
cat("Bundle class:", class(bundle_direct), "\n")
cat("Bundle contains:\n")
print(names(bundle_direct))

# Access key components
cat("\nMCMC settings:\n")
print(bundle_direct$mcmc_settings)

## ----mcmc-basic---------------------------------------------------------------
# Run MCMC
fit <- run_mcmc_bundle_manual(bundle_direct, show_progress = FALSE)

cat("Fit object class:", class(fit), "\n")
cat("MCMC execution complete. Posterior samples collected.\n")

## ----mcmc-posterior-----------------------------------------------------------
# Posterior summary
print("\n--- POSTERIOR SUMMARY ---\n")
summary(fit)

# Posterior mean parameters in original form
params_fit <- params(fit)
params_fit

## ----mcmc-diagnostics---------------------------------------------------------
# Trace plots
plot(fit, params = "alpha|beta", family = c("traceplot", "running", "autocorrelation"))

## ----workflow-complete--------------------------------------------------------
# Load packaged data
data("nc_pos200_k3")
y_data <- nc_pos200_k3$y

# PHASE 1: Bundle
bundle_final <- build_nimble_bundle(
  y = y_data,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  components = 5,
  mcmc = list(
    niter = 120,
    nburnin = 20,
    nchains = 1,
    thin = 1
  )
)

# PHASE 2: MCMC
fit_final <- run_mcmc_bundle_manual(bundle_final, show_progress = FALSE)

print("\n=== THREE-PHASE WORKFLOW COMPLETE ===\n")
summary(fit_final)

## ----backend-crp--------------------------------------------------------------
# Chinese Restaurant Process
bundle_crp <- build_nimble_bundle(
  y = y_data,
  kernel = "gamma",
  backend = "crp",
  components = 5,
  mcmc = list(niter = 200, nburnin = 50, nchains = 1)
)

fit_crp <- run_mcmc_bundle_manual(bundle_crp, show_progress = FALSE)
print("CRP execution complete.\n")

## ----backend-sb---------------------------------------------------------------
# Stick-Breaking Process
bundle_sb <- build_nimble_bundle(
  y = y_data,
  kernel = "gamma",
  backend = "sb",
  components = 5,
  mcmc = list(niter = 120, nburnin = 20, nchains = 1)
)

fit_sb <- run_mcmc_bundle_manual(bundle_sb, show_progress = FALSE)
print("SB execution complete.\n")

## ----kernel-guide-------------------------------------------------------------
kernels_available <- c("gamma", "lognormal", "normal", "laplace", "invgauss", "amoroso", "cauchy")

cat("Available kernels:\n")
for (k in kernels_available) {
  cat("  -", k, "\n")
}

print("\nChoose kernel based on:\n")
print("  gamma:     Right-skewed, positive support\n")
print("  lognormal: Log-transformed normality\n")
print("  normal:    Symmetric, unbounded\n")
print("  laplace:   Sharp peak, exponential tails\n")
print("  invgauss:  Positive, near-normal shape\n")
print("  amoroso:   Generalized, maximum flexibility\n")
print("  cauchy:    Heavy-tailed, rare cases\n")

## ----gpd-example, eval=FALSE--------------------------------------------------
# # Data with tail behavior
# data("nc_pos_tail200_k4")
# y_tail <- nc_pos_tail200_k4$y
# 
# # Build with GPD
# bundle_gpd <- build_nimble_bundle(
#   y = y_tail,
#   kernel = "gamma",
#   backend = "sb",
#   GPD = TRUE,
#   components = 6,
#   mcmc = list(niter = 200, nburnin = 50, nchains = 1, waic = FALSE)
# )
# 
# fit_gpd <- run_mcmc_bundle_manual(bundle_gpd, show_progress = FALSE)
# print("\nGPD augmentation applied to tail region.\n")
# summary(fit_gpd)

## ----param-reference----------------------------------------------------------
print("=== Recommended MCMC Parameters ===\n")
print("Quick test:     niter=500,  nburnin=100, nchains=1\n")
print("Standard:       niter=1000, nburnin=250, nchains=2\n")
print("Production:     niter=1000, nburnin=250, nchains=3\n")
print("\n=== Backend Parameters ===\n")
print("Use components=3-5 for both backends in this implementation.\n")
print("\n=== Kernel Selection ===\n")
print("Positive data:     gamma, lognormal, invgauss\n")
print("Any real data:     normal\n")
print("Symmetric tails:   laplace\n")
print("Extreme outliers:  cauchy\n")

