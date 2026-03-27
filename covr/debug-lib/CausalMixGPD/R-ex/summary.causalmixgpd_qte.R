### Name: summary.causalmixgpd_qte
### Title: Summarize a QTE-style effect object
### Aliases: summary.causalmixgpd_qte

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
##D summary(q)
## End(Not run)



