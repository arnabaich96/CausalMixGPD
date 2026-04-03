pkgload::load_all('d:/Git-Repos/CausalMixGPD-Package/CausalMixGPD', quiet=TRUE, export_all=FALSE)
data('lalonde', package='MatchIt')
covars <- within(lalonde, {
  race <- factor(race, levels = c('white', 'black', 'hispan'))
  married <- factor(married, levels = c(0, 1), labels = c('no', 'yes'))
  nodegree <- factor(nodegree, levels = c(0, 1), labels = c('no', 'yes'))
})
X <- stats::model.matrix(~ age + educ + race + married + nodegree + re74 + re75, data = covars)[, -1, drop = FALSE]
y <- covars$re78 + 0.1
A <- as.integer(covars$treat)
mcmc_fixed <- list(niter = 250, nburnin = 50, thin = 1, nchains = 1, seed = 2026)
fit <- dpmgpd.causal(y = y, X = X, treat = A, backend = 'sb', kernel = 'gamma', components = 10,
                     mcmc = mcmc_fixed, PS = 'logit', parallel_arms = FALSE)
cat('fit built:', !is.null(fit$outcome_fit$con), '\n')
