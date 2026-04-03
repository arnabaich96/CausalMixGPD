test_that("run_mcmc_causal forwards z_update_every to both arms", {
  skip_if_not_test_level("ci")
  set.seed(521)
  n <- 100
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  A <- stats::rbinom(n, 1, 0.5)
  y <- abs(0.3 * X[, 1] + A + stats::rnorm(n)) + 0.1

  cb <- build_causal_bundle(
    y = y, X = X, A = A,
    backend = "sb", kernel = "normal", GPD = FALSE, components = 6,
    mcmc_outcome = list(niter = 90, nburnin = 20, thin = 1, nchains = 1, seed = 15),
    mcmc_ps = list(niter = 70, nburnin = 20, thin = 1, nchains = 1, seed = 16),
    PS = "logit"
  )
  fit <- run_mcmc_causal(cb, show_progress = FALSE, z_update_every = 3)
  expect_equal(fit$outcome_fit$con$mcmc$z_update_every, 3L)
  expect_equal(fit$outcome_fit$trt$mcmc$z_update_every, 3L)
})
