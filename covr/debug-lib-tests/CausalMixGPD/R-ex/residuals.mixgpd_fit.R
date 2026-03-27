### Name: residuals.mixgpd_fit
### Title: Residual diagnostics on the training design
### Aliases: residuals.mixgpd_fit

### ** Examples

## Not run: 
##D y <- abs(stats::rnorm(50)) + 0.1
##D X <- data.frame(x1 = stats::rnorm(50), x2 = stats::runif(50))
##D bundle <- build_nimble_bundle(y = y, X = X, backend = "sb", kernel = "lognormal",
##D                              GPD = FALSE, components = 4,
##D                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
##D fit <- run_mcmc_bundle_manual(bundle)
##D pit_plugin <- residuals(fit, type = "pit", pit = "plugin")
##D pit_bayes_mean <- residuals(fit, type = "pit", pit = "bayes_mean", pit_seed = 1L)
##D pit_bayes_draw <- residuals(fit, type = "pit", pit = "bayes_draw", pit_seed = 1L)
##D attr(pit_bayes_draw, "pit_diagnostics")
## End(Not run)



