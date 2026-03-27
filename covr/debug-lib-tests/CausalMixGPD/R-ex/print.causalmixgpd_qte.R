### Name: print.causalmixgpd_qte
### Title: Print a QTE-style effect object
### Aliases: print.causalmixgpd_qte

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
##D print(q)
## End(Not run)



