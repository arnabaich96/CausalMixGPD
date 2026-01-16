test_that("GPD splice is continuous at the threshold", {
  w <- c(0.3, 0.7)
  mean <- c(-1, 1)
  sd <- c(0.5, 1)
  threshold <- 0.2
  tail_scale <- 1.0
  tail_shape <- 0.2

  p_bulk <- pNormMix(threshold, w = w, mean = mean, sd = sd,
                     lower.tail = TRUE, log.p = FALSE)
  p_splice <- pNormMixGpd(threshold, w = w, mean = mean, sd = sd,
                          threshold = threshold, tail_scale = tail_scale,
                          tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE)
  expect_equal(p_splice, p_bulk, tolerance = 1e-6)
})

test_that("Tail dominates at high quantiles with GPD", {
  w <- c(0.3, 0.7)
  mean <- c(-1, 1)
  sd <- c(0.5, 1)
  threshold <- 0.2
  # Make the tail appreciably heavier so the GPD splice should inflate extremes
  tail_scale <- 2.5
  tail_shape <- 0.5

  q_bulk <- qNormMix(0.99, w = w, mean = mean, sd = sd)
  q_tail <- qNormMixGpd(0.99, w = w, mean = mean, sd = sd,
                        threshold = threshold, tail_scale = tail_scale,
                        tail_shape = tail_shape)
  expect_gt(q_tail, q_bulk)
})

test_that("SB and CRP predictions agree in a small synthetic case", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  set.seed(42)
  y <- abs(stats::rnorm(20)) + 0.1
  mcmc_cfg <- list(niter = 40, nburnin = 10, thin = 1, nchains = 1, seed = 1)

  sb_bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6,
    mcmc = mcmc_cfg
  )
  crp_bundle <- build_nimble_bundle(
    y = y,
    backend = "crp",
    kernel = "normal",
    GPD = FALSE,
    components = 6,
    mcmc = mcmc_cfg
  )

  sb_fit <- run_mcmc_bundle_manual(sb_bundle, show_progress = FALSE)
  crp_fit <- run_mcmc_bundle_manual(crp_bundle, show_progress = FALSE)

  q_sb <- predict(sb_fit, type = "quantile", index = 0.5)$fit[1, 1]
  q_crp <- predict(crp_fit, type = "quantile", index = 0.5)$fit[1, 1]
  expect_lt(abs(q_sb - q_crp), 1.0)
})

test_that("GPD on/off changes high-quantile behavior", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  set.seed(123)
  y <- abs(stats::rnorm(20)) + 0.1
  mcmc_cfg <- list(niter = 40, nburnin = 10, thin = 1, nchains = 1, seed = 2)

  fit_off <- run_mcmc_bundle_manual(build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6,
    mcmc = mcmc_cfg
  ), show_progress = FALSE)

  fit_on <- run_mcmc_bundle_manual(build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = TRUE,
    components = 6,
    mcmc = mcmc_cfg
  ), show_progress = FALSE)

  q_off <- predict(fit_off, type = "quantile", index = 0.99)$fit[1, 1]
  q_on <- predict(fit_on, type = "quantile", index = 0.99)$fit[1, 1]
  # Turning on the GPD tail should noticeably alter the high-quantile prediction,
  # even if the direction varies with the small synthetic sample and short chain.
  expect_gt(abs(q_on - q_off), 0.1)
})
