### Name: qtt
### Title: Quantile treatment effects standardized to treated covariates
### Aliases: qtt

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D qtt(fit, probs = c(0.5, 0.9))
## End(Not run)



