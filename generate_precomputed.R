# Script to generate precomputed files for legacy vignettes
# Run this before pkgdown::build_site() to ensure all precomputed files exist

library(DPmixGPD)

# Set FAST mode
FAST <- TRUE
mcmc_fast <- list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)
mcmc <- mcmc_fast

# Source the setup file
source("vignettes/legacy/_legacy-setup.R")

# Generate v04 precomputed file
cat("Generating v04-backends-and-workflow-fit.rds...\n")
tryCatch({
  bundle <- build_nimble_bundle(
    y = rnorm(50),
    backend = "crp",
    kernel = "normal",
    GPD = FALSE,
    components = 5,
    mcmc = mcmc
  )
  fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
  saveRDS(fit, "vignettes/legacy/articles/legacy-precomputed/v04-backends-and-workflow-fit.rds")
  cat("✓ Generated v04-backends-and-workflow-fit.rds\n")
}, error = function(e) {
  cat("✗ Failed to generate v04 file:", conditionMessage(e), "\n")
})

cat("Precomputed file generation complete.\n")
