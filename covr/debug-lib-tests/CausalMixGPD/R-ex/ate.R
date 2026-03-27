### Name: ate
### Title: Average treatment effects, marginal over the empirical covariate
###   distribution
### Aliases: ate

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D ate(fit, interval = "credible", level = 0.90, nsim_mean = 100)
## End(Not run)



