### Name: qte
### Title: Quantile treatment effects, marginal over the empirical
###   covariate distribution
### Aliases: qte

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D qte(fit, probs = c(0.5, 0.9))
## End(Not run)



