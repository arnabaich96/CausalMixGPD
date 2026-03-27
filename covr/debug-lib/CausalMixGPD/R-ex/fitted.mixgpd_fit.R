### Name: fitted.mixgpd_fit
### Title: Fitted values on the training design
### Aliases: fitted.mixgpd_fit

### ** Examples

## Not run: 
##D # Conditional model (with covariates X)
##D y <- abs(stats::rnorm(50)) + 0.1
##D X <- data.frame(x1 = stats::rnorm(50), x2 = stats::runif(50))
##D bundle <- build_nimble_bundle(y = y, X = X, backend = "sb", kernel = "normal",
##D                              GPD = TRUE, components = 6,
##D                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
##D fit <- run_mcmc_bundle_manual(bundle)
##D fitted(fit)
##D fitted(fit, level = 0.90)
##D fitted(fit, interval = "hpd")  # HPD intervals
##D fitted(fit, interval = NULL)   # No intervals
## End(Not run)



