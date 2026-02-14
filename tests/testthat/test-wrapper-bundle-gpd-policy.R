test_that("dpmgpd(non_gpd_bundle) errors with a clear message", {
  set.seed(1001)
  y <- abs(stats::rnorm(20)) + 0.1

  b <- DPmixGPD::build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    mcmc = mcmc_fast(seed = 1L)
  )

  expect_error(
    DPmixGPD::dpmgpd(b, mcmc = list(show_progress = FALSE)),
    "requires a bundle with GPD enabled"
  )
})

test_that("dpmgpd(causal bundle with any non-GPD arm) errors", {
  set.seed(1002)
  n <- 24
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  A <- rep(c(0L, 1L), length.out = n)
  y <- abs(stats::rnorm(n)) + 0.1

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    A = A,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(TRUE, FALSE),
    components = c(4, 4),
    PS = FALSE,
    mcmc_outcome = mcmc_fast(seed = 2L)
  )

  expect_error(
    DPmixGPD::dpmgpd(cb, mcmc = list(show_progress = FALSE)),
    "requires a bundle with GPD enabled"
  )
})

test_that("dpmix(gpd non-causal bundle) strips GPD then runs", {
  skip_if_not_test_level("ci")

  set.seed(1003)
  y <- abs(stats::rnorm(25)) + 0.1

  b <- DPmixGPD::build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = TRUE,
    components = 4,
    mcmc = mcmc_fast(seed = 3L)
  )

  fit <- DPmixGPD::dpmix(
    b,
    mcmc = list(
      niter = 20L,
      nburnin = 5L,
      thin = 1L,
      nchains = 1L,
      seed = 3L,
      show_progress = FALSE,
      quiet = TRUE
    )
  )

  expect_s3_class(fit, "mixgpd_fit")
  expect_false(isTRUE(fit$spec$meta$GPD))
  expect_false(isTRUE(fit$spec$plan$GPD))
})

test_that("dpmix(gpd causal bundle) strips GPD for both arms then runs", {
  skip_if_not_test_level("ci")

  set.seed(1004)
  n <- 28
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n))
  A <- rep(c(0L, 1L), length.out = n)
  y <- abs(stats::rnorm(n)) + 0.1

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    A = A,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(TRUE, TRUE),
    components = c(4, 4),
    PS = FALSE,
    mcmc_outcome = mcmc_fast(seed = 4L)
  )

  fit <- DPmixGPD::dpmix(
    cb,
    mcmc = list(
      niter = 20L,
      nburnin = 5L,
      thin = 1L,
      nchains = 1L,
      seed = 4L,
      show_progress = FALSE
    )
  )

  expect_s3_class(fit, "dpmixgpd_causal_fit")
  expect_false(isTRUE(fit$bundle$outcome$con$spec$meta$GPD))
  expect_false(isTRUE(fit$bundle$outcome$trt$spec$meta$GPD))
  expect_false(isTRUE(fit$bundle$outcome$con$spec$plan$GPD))
  expect_false(isTRUE(fit$bundle$outcome$trt$spec$plan$GPD))
})

test_that("dpmix(non-GPD bundle) and dpmgpd(all-GPD bundle) both run", {
  skip_if_not_test_level("ci")

  set.seed(1005)
  y <- abs(stats::rnorm(22)) + 0.1

  b_mix <- DPmixGPD::build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    mcmc = mcmc_fast(seed = 5L)
  )
  b_gpd <- DPmixGPD::build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = TRUE,
    components = 4,
    mcmc = mcmc_fast(seed = 6L)
  )

  fit_mix <- DPmixGPD::dpmix(
    b_mix,
    mcmc = list(
      niter = 20L,
      nburnin = 5L,
      thin = 1L,
      nchains = 1L,
      seed = 5L,
      show_progress = FALSE,
      quiet = TRUE
    )
  )
  fit_gpd <- DPmixGPD::dpmgpd(
    b_gpd,
    mcmc = list(
      niter = 20L,
      nburnin = 5L,
      thin = 1L,
      nchains = 1L,
      seed = 6L,
      show_progress = FALSE,
      quiet = TRUE
    )
  )

  expect_s3_class(fit_mix, "mixgpd_fit")
  expect_s3_class(fit_gpd, "mixgpd_fit")
  expect_false(isTRUE(fit_mix$spec$meta$GPD))
  expect_true(isTRUE(fit_gpd$spec$meta$GPD))
})
