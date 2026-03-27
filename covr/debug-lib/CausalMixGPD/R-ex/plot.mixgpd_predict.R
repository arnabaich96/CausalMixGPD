### Name: plot.mixgpd_predict
### Title: Plot prediction results
### Aliases: plot.mixgpd_predict

### ** Examples

## Not run: 
##D y <- abs(stats::rnorm(50)) + 0.1
##D bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
##D                              GPD = TRUE, components = 6,
##D                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
##D fit <- run_mcmc_bundle_manual(bundle)
##D 
##D # Quantile prediction with plot
##D pred_q <- predict(fit, type = "quantile", index = c(0.25, 0.5, 0.75))
##D plot(pred_q)
##D 
##D # Sample prediction with plot
##D pred_s <- predict(fit, type = "sample", nsim = 500)
##D plot(pred_s)
##D 
##D # Mean prediction with plot
##D pred_m <- predict(fit, type = "mean", nsim_mean = 300)
##D plot(pred_m)
## End(Not run)



