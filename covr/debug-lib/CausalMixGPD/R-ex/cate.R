### Name: cate
### Title: Conditional average treatment effects
### Aliases: cate

### ** Examples

## Not run: 
##D cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
##D fit <- run_mcmc_causal(cb, show_progress = FALSE)
##D cate(fit, newdata = X[1:5, ])
##D cate(fit, interval = "credible", level = 0.90)  # 90% CI
##D cate(fit, interval = "hpd")  # HPD intervals
##D cate(fit, interval = NULL)   # No intervals
## End(Not run)



