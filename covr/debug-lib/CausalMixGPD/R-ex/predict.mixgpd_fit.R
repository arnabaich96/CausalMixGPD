### Name: predict.mixgpd_fit
### Title: Posterior predictive summaries from a fitted one-arm model
### Aliases: predict.mixgpd_fit

### ** Examples

## Not run: 
##D y <- abs(stats::rnorm(50)) + 0.1
##D bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
##D                              GPD = TRUE, components = 6,
##D                              mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
##D fit <- run_mcmc_bundle_manual(bundle)
##D pr <- predict(fit, type = "quantile", p = c(0.5, 0.9))
##D pr_surv <- predict(fit, y = sort(y), type = "survival")
##D pr_cdf <- list(fit = 1 - pr_surv$fit)
##D # HPD intervals
##D pr_hpd <- predict(fit, type = "quantile", p = c(0.5, 0.9), interval = "hpd")
##D # No intervals
##D pr_none <- predict(fit, type = "quantile", p = c(0.5, 0.9), interval = NULL)
##D # Restricted mean (finite under heavy tails)
##D pr_rmean <- predict(fit, type = "rmean", cutoff = 10, interval = "credible")
## End(Not run)



