### Name: summary.causalmixgpd_ate
### Title: Summarize an ATE-style effect object
### Aliases: summary.causalmixgpd_ate

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D a <- ate(fit, interval = "credible")
##D summary(a)
## End(Not run)



