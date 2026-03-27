### Name: predict.causalmixgpd_causal_fit
### Title: Predict arm-specific and contrast-scale quantities from a causal
###   fit
### Aliases: predict.causalmixgpd_causal_fit

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
##D fit <- run_mcmc_causal(cb)
##D predict(fit, x = X[1:10, ], type = "quantile", index = c(0.25, 0.5, 0.75))
##D predict(fit, x = X[1:10, ], type = "mean", interval = "hpd")  # HPD intervals
##D predict(fit, x = X[1:10, ], type = "mean", interval = NULL)   # No intervals
## End(Not run)



