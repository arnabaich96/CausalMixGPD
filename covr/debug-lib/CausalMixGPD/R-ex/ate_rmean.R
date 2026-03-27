### Name: ate_rmean
### Title: Restricted-mean ATE helper
### Aliases: ate_rmean

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal",
##D                          GPD = TRUE, components = 6)
##D fit <- run_mcmc_causal(cb)
##D ate_rm <- ate_rmean(fit, cutoff = 10, interval = "credible")
## End(Not run)



