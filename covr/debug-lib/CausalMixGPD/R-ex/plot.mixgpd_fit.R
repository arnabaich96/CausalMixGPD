### Name: plot.mixgpd_fit
### Title: Plot MCMC diagnostics for a MixGPD fit (ggmcmc backend)
### Aliases: plot.mixgpd_fit

### ** Examples

## Not run: 
##D y <- abs(stats::rnorm(50)) + 0.1
##D bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
##D                              GPD = TRUE, components = 6,
##D                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
##D fit <- run_mcmc_bundle_manual(bundle)
##D plot(fit, family = c("traceplot", "density"))
## End(Not run)



