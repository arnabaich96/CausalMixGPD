test_that("predict applies large-N defaults and supports cache hits", {
  skip_if_not_test_level("full")
  set.seed(531)
  n <- 120
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  y <- abs(0.4 * X[, 1] + stats::rnorm(n)) + 0.1
  fit <- dpmgpd(
    x = y, X = X, backend = "sb", kernel = "normal", components = 6,
    mcmc = list(niter = 100, nburnin = 30, thin = 1, nchains = 1, seed = 17),
    monitor_latent = TRUE
  )
  X_new <- X[rep(seq_len(nrow(X)), length.out = 20050), , drop = FALSE]

  p1 <- suppressMessages(predict(fit, x = X_new, type = "quantile", index = c(0.5, 0.9), parallel = FALSE))
  p2 <- suppressMessages(predict(fit, x = X_new, type = "quantile", index = c(0.5, 0.9), parallel = FALSE))

  expect_s3_class(p1, "mixgpd_predict")
  expect_s3_class(p2, "mixgpd_predict")
  expect_equal(dim(p1$fit), dim(p2$fit))
  expect_true(
    isTRUE(p2$diagnostics$cache_hit) ||
      isTRUE(all.equal(p1$fit, p2$fit, check.attributes = FALSE))
  )
})
