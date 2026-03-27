### Name: run_mcmc_bundle_manual
### Title: Run posterior sampling for a prepared one-arm bundle
### Aliases: run_mcmc_bundle_manual

### ** Examples

## Not run: 
##D library(nimble)
##D y <- abs(rnorm(40)) + 0.1
##D bundle <- build_nimble_bundle(
##D   y = y,
##D   backend = "sb",
##D   kernel = "normal",
##D   GPD = FALSE,
##D   components = 3,
##D   mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
##D )
##D fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
##D fit
## End(Not run)



