test_that("Bundle generation: minimal y only", {
  skip_if_not_installed("nimble")
  skip_on_cran()

  set.seed(1)
  y <- abs(stats::rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6
  )

  expect_true(inherits(bundle, "dpmixgpd_bundle"), info = "bundle class (y only)")
  expect_true(is.list(bundle$spec$meta), info = "bundle spec meta exists (y only)")
  expect_false(isTRUE(bundle$spec$meta$has_X), info = "has_X is FALSE (y only)")
})

test_that("Bundle generation: with X", {
  skip_if_not_installed("nimble")
  skip_on_cran()

  set.seed(1)
  y <- abs(stats::rnorm(20)) + 0.1
  X <- data.frame(x1 = stats::rnorm(20), x2 = stats::runif(20))
  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6
  )

  expect_true(inherits(bundle, "dpmixgpd_bundle"), info = "bundle class (with X)")
  expect_true(isTRUE(bundle$spec$meta$has_X), info = "has_X is TRUE (with X)")
  expect_equal(bundle$spec$meta$P, ncol(X), info = "P matches ncol(X)")
})

test_that("Bundle generation: custom param_specs", {
  skip_if_not_installed("nimble")
  skip_on_cran()

  set.seed(1)
  y <- abs(stats::rnorm(20)) + 0.1
  X <- data.frame(x1 = stats::rnorm(20), x2 = stats::runif(20))

  param_specs <- list(
    bulk = list(
      mean = list(mode = "link", link = "identity")
    ),
    gpd = list(
      threshold = list(mode = "link", link = "identity"),
      tail_scale = list(mode = "link", link = "exp")
    )
  )

  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "normal",
    GPD = TRUE,
    components = 6,
    param_specs = param_specs
  )

  plan <- bundle$spec$plan
  expect_equal(plan$bulk$mean$mode, "link", info = "bulk mean link mode")
  expect_equal(plan$gpd$threshold$mode, "link", info = "threshold link mode")
  expect_equal(plan$gpd$tail_scale$mode, "link", info = "tail_scale link mode")
})

test_that("Bundle generation: custom mcmc args", {
  skip_if_not_installed("nimble")
  skip_on_cran()

  set.seed(1)
  y <- abs(stats::rnorm(20)) + 0.1
  mcmc_args <- list(niter = 40, nburnin = 10, thin = 1, nchains = 1, seed = 1)

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6,
    mcmc = mcmc_args
  )

  expect_equal(bundle$mcmc$niter, 40, info = "mcmc niter stored")
  expect_equal(bundle$mcmc$nburnin, 10, info = "mcmc nburnin stored")
})
