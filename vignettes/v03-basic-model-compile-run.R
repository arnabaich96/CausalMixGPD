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
  mcmc = list(niter = 1500, nburnin = 250, nchains = 2)
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
fit <- run_mcmc_bundle_manual(bundle_direct)

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
    niter = 60,
    nburnin = 10,
    nchains = 2,
    thin = 1
  )
)

# PHASE 2: MCMC
fit_final <- run_mcmc_bundle_manual(bundle_final)

print("\n=== THREE-PHASE WORKFLOW COMPLETE ===\n")
summary(fit_final)

