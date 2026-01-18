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

