# test-causal-ate.R
# ATE/QTE draw alignment tests (Tier B - requires MCMC)

test_that("ATE/QTE use matching posterior draws and shapes", {
  skip_if_not_test_level("ci")

  set.seed(42)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)
  mcmc_ps  <- list(niter = 20, nburnin = 5, thin = 1, nchains = 1, seed = 1)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  newx <- head(X, 3)

  # QTE: draws should align across arms and differencing happens per draw
  qres <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = newx, interval = "credible")
  expect_true(is.matrix(qres$fit) && all(dim(qres$fit) == c(nrow(newx), 2)))
  expect_true(identical(dim(qres$trt$draws), dim(qres$con$draws)))
  diff_q <- qres$trt$draws - qres$con$draws
  expect_equal(apply(diff_q, c(2, 3), mean), qres$fit, tolerance = 1e-8)
  expect_true(all(qres$lower <= qres$upper, na.rm = TRUE))

  # ATE: mean differences per draw
  ares <- DPmixGPD::ate(cf, newdata = newx, interval = "credible", nsim_mean = 20L)
  expect_true(length(ares$fit) == nrow(newx))
  expect_true(identical(dim(ares$trt$draws), dim(ares$con$draws)))
  expect_true(is.null(ares$grid))
  diff_a <- ares$trt$draws - ares$con$draws
  expect_equal(colMeans(diff_a), ares$fit, tolerance = 1e-8)
  expect_true(all(ares$lower <= ares$upper, na.rm = TRUE))
})
