### Name: att
### Title: Average treatment effects standardized to treated covariates
### Aliases: att

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D att(fit, interval = "credible", nsim_mean = 100)
## End(Not run)



