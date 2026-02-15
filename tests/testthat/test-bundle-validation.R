# tests/testthat/test-bundle-validation.R
# Unit tests for build_nimble_bundle input validation and edge cases

# ======================================================================
# Input validation tests for build_nimble_bundle
# ======================================================================

# Access build_nimble_bundle
build_nimble_bundle <- CausalMixGPD::build_nimble_bundle

test_that("build_nimble_bundle errors on empty y", {
  expect_error(
    build_nimble_bundle(
      y = numeric(0),
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4
    ),
    regexp = "non-empty"
  )
})

test_that("build_nimble_bundle errors on NULL y", {
  expect_error(
    build_nimble_bundle(
      y = NULL,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4
    ),
    regexp = "non-empty|length"
  )
})

test_that("build_nimble_bundle errors on components < 2", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  expect_error(
    build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 1
    ),
    regexp = ">= 2"
  )
})

test_that("build_nimble_bundle errors on invalid epsilon", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1

  expect_error(
    build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,
      epsilon = -0.1
    ),
    regexp = "epsilon|\\[0.*1\\)"
  )

  expect_error(
    build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,
      epsilon = 1.5
    ),
    regexp = "epsilon|\\[0.*1\\)"
  )

  expect_error(
    build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4,
      epsilon = NA
    ),
    regexp = "epsilon|numeric"
  )
})

test_that("build_nimble_bundle errors on ps length mismatch", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  ps <- runif(10)  # wrong length

  expect_error(
    build_nimble_bundle(
      y = y,
      ps = ps,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 4
    ),
    regexp = "same length"
  )
})

# ======================================================================
# Bundle structure tests
# ======================================================================

test_that("build_nimble_bundle returns correct class", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )
  expect_s3_class(bundle, "causalmixgpd_bundle")
})

test_that("build_nimble_bundle has required components", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )

  expect_true("spec" %in% names(bundle))
  expect_true("code" %in% names(bundle))
  expect_true("constants" %in% names(bundle))
  expect_true("data" %in% names(bundle))
  expect_true("inits" %in% names(bundle))
  expect_true("monitors" %in% names(bundle))
  expect_true("mcmc" %in% names(bundle))
  expect_true("epsilon" %in% names(bundle))
})

test_that("build_nimble_bundle stores epsilon value", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    epsilon = 0.05
  )
  expect_equal(bundle$epsilon, 0.05)
})

test_that("build_nimble_bundle stores custom MCMC settings", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    mcmc = list(niter = 100, nburnin = 20, thin = 2, nchains = 2, seed = 42)
  )

  expect_equal(bundle$mcmc$niter, 100)
  expect_equal(bundle$mcmc$nburnin, 20)
  expect_equal(bundle$mcmc$thin, 2)
  expect_equal(bundle$mcmc$nchains, 2)
  expect_equal(bundle$mcmc$seed, 42)
})

# ======================================================================
# Backend-specific tests
# ======================================================================

test_that("build_nimble_bundle works with CRP backend", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "crp",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
  expect_equal(bundle$spec$meta$backend, "crp")
})

test_that("build_nimble_bundle works with GPD tail", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = TRUE,
    components = 4
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
  expect_true(bundle$spec$meta$GPD)
})

# ======================================================================
# Kernel-specific tests
# ======================================================================

test_that("build_nimble_bundle works with all kernels (sb, no GPD)", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  kernels <- c("normal", "lognormal", "gamma", "invgauss", "laplace", "amoroso", "cauchy")

  for (k in kernels) {
    bundle <- build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = k,
      GPD = FALSE,
      components = 4
    )
    expect_s3_class(bundle, "causalmixgpd_bundle")
    expect_equal(bundle$spec$meta$kernel, k)
  }
})

test_that("build_nimble_bundle works with X matrix", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  X <- matrix(rnorm(40), ncol = 2)
  colnames(X) <- c("x1", "x2")

  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
  expect_true(bundle$spec$meta$has_X)
  expect_equal(bundle$spec$meta$P, 2)
})

test_that("build_nimble_bundle converts data.frame X to matrix", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  X <- data.frame(x1 = rnorm(20), x2 = runif(20))

  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
  expect_true(bundle$spec$meta$has_X)
})

# ======================================================================
# alpha_random parameter tests
# ======================================================================

test_that("build_nimble_bundle respects alpha_random = FALSE", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    alpha_random = FALSE
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
})

test_that("build_nimble_bundle respects alpha_random = TRUE", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    alpha_random = TRUE
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
})

# ======================================================================
# param_specs override tests
# ======================================================================

test_that("build_nimble_bundle applies param_specs overrides", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  X <- matrix(rnorm(40), ncol = 2)
  colnames(X) <- c("x1", "x2")

  param_specs <- list(
    bulk = list(
      mean = list(mode = "link", link = "identity"),
      sd = list(mode = "dist")
    )
  )

  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    param_specs = param_specs
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
  expect_equal(bundle$spec$plan$bulk$mean$mode, "link")
})

# ======================================================================
# J parameter alias tests
# ======================================================================

test_that("build_nimble_bundle uses components parameter correctly", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 6
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
  expect_equal(bundle$spec$meta$components, 6)
})

# ======================================================================
# PS augmentation tests
# ======================================================================

test_that("build_nimble_bundle works with propensity score vector", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  ps <- runif(20)

  bundle <- build_nimble_bundle(
    y = y,
    ps = ps,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
  # PS should be included in data
  expect_true("ps" %in% names(bundle$data))
})

test_that("build_nimble_bundle works with X and ps combined", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  X <- matrix(rnorm(40), ncol = 2)
  ps <- runif(20)

  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    ps = ps,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )

  expect_s3_class(bundle, "causalmixgpd_bundle")
  expect_true(bundle$spec$meta$has_X)
})
