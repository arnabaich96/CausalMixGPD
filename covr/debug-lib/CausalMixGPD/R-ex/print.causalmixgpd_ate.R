### Name: print.causalmixgpd_ate
### Title: Print an ATE-style effect object
### Aliases: print.causalmixgpd_ate

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D a <- ate(fit, interval = "credible")
##D print(a)
## End(Not run)



