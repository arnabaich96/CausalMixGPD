### Name: summary.mixgpd_fit
### Title: Summarize posterior draws from a one-arm fitted model
### Aliases: summary.mixgpd_fit

### ** Examples

## Not run: 
##D y <- abs(stats::rnorm(50)) + 0.1
##D bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
##D                              GPD = TRUE, components = 6,
##D                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
##D fit <- run_mcmc_bundle_manual(bundle)
##D summary(fit, pars = c("alpha", "threshold"))
## End(Not run)



