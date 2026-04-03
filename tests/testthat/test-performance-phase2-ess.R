test_that("ess_summary returns stable structure for mixgpd_fit", {
  skip_if_not_test_level("ci")
  set.seed(501)
  y <- abs(stats::rnorm(90)) + 0.1
  b <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6,
    mcmc = list(niter = 120, nburnin = 30, thin = 1, nchains = 1, seed = 11, timing = TRUE)
  )
  fit <- run_mcmc_bundle_manual(b, show_progress = FALSE, timing = TRUE)
  es <- ess_summary(fit)

  expect_s3_class(es, "mixgpd_ess_summary")
  expect_true(all(c("table", "overall", "meta") %in% names(es)))
  expect_true(all(c("param", "chain", "ess", "seconds", "ess_per_sec") %in% names(es$table)))
  expect_true(nrow(es$table) >= 1L)
})

test_that("ess_summary supports causal fit arm-wise summaries", {
  skip_if_not_test_level("ci")
  set.seed(502)
  n <- 120
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  A <- stats::rbinom(n, 1, 0.5)
  y <- abs(0.4 * X[, 1] + A + stats::rnorm(n)) + 0.1
  cb <- build_causal_bundle(
    y = y, X = X, A = A,
    backend = "sb", kernel = "normal", GPD = FALSE, components = 6,
    mcmc_outcome = list(niter = 100, nburnin = 30, thin = 1, nchains = 1, seed = 12),
    mcmc_ps = list(niter = 80, nburnin = 20, thin = 1, nchains = 1, seed = 13),
    PS = "logit"
  )
  fit <- run_mcmc_causal(cb, show_progress = FALSE, timing = TRUE)
  es <- ess_summary(fit)
  expect_true(all(c("control", "treated") %in% unique(es$table$arm)))
})
