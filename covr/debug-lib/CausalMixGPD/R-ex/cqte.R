### Name: cqte
### Title: Conditional quantile treatment effects
### Aliases: cqte

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D cqte(fit, probs = c(0.5, 0.9), newdata = X[1:5, ])
##D cqte(fit, probs = c(0.5, 0.9), interval = "credible", level = 0.90)  # 90% CI
##D cqte(fit, probs = c(0.5, 0.9), interval = "hpd")  # HPD intervals
##D cqte(fit, probs = c(0.5, 0.9), interval = NULL)   # No intervals
## End(Not run)



