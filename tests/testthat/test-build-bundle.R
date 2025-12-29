testthat::test_that("SB lognormal bundles build successfully", {
  testthat::skip_if_not_installed("nimble")

  set.seed(123)
  y <- abs(stats::rnorm(40)) + 0.2

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "lognormal",
    components = 4,
    GPD = FALSE,
    mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 1, seed = 99)
  )

  testthat::expect_s3_class(bundle, "dpmixgpd_bundle")
  testthat::expect_s3_class(bundle$code, "nimbleCode")
  testthat::expect_identical(bundle$spec$meta$kernel, "lognormal")
  testthat::expect_identical(bundle$spec$meta$backend, "sb")
  testthat::expect_identical(bundle$spec$meta$J, 4L)
})
