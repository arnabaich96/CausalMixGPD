# tests/testthat/test-causal-validation.R
# Unit tests for build_causal_bundle input validation and edge cases

# ======================================================================
# Input validation tests for build_causal_bundle
# ======================================================================

# Access build_causal_bundle
build_causal_bundle <- DPmixGPD::build_causal_bundle

test_that("build_causal_bundle errors on empty y", {
  expect_error(
    build_causal_bundle(
      y = numeric(0),
      X = matrix(1:2, ncol = 1),
      T = c(0, 1),
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      design = "rct"
    ),
    regexp = "non-empty"
  )
})

test_that("build_causal_bundle errors on T length mismatch", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- rbinom(10, 1, 0.5)  # wrong length

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      design = "rct"
    ),
    regexp = "same length"
  )
})

test_that("build_causal_bundle errors on non-binary T", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- rep(c(0, 1, 2), length.out = 20)  # not binary

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      design = "rct"
    ),
    regexp = "binary|0/1"
  )
})

test_that("build_causal_bundle errors on T with NA values", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 9), NA, rep(1, 10))

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      design = "rct"
    ),
    regexp = "binary|NA"
  )
})

test_that("build_causal_bundle errors when only one treatment arm has data", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- rep(1, 20)  # all treated, no control

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      design = "rct"
    ),
    regexp = "Both treatment arms|at least one"
  )
})

test_that("build_causal_bundle errors when X row mismatch", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(30), ncol = 3)  # 10 rows instead of 20
  T <- rbinom(20, 1, 0.5)

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      design = "rct"
    ),
    regexp = "same number of rows"
  )
})

test_that("build_causal_bundle errors when both J and components provided", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      components = 6,
      design = "rct"
    ),
    regexp = "only one of"
  )
})

test_that("build_causal_bundle errors on components < 2", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 1,
      design = "rct"
    ),
    regexp = ">= 2"
  )
})

test_that("build_causal_bundle errors on invalid epsilon", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      epsilon = -0.1,
      design = "rct"
    ),
    regexp = "epsilon|\\[0.*1\\)"
  )
})

# ======================================================================
# Design-specific validation tests
# ======================================================================

test_that("build_causal_bundle warns and ignores PS for RCT design", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  expect_warning(
    bundle <- build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      design = "rct",
      PS = "logit"  # Should be ignored with warning
    ),
    regexp = "ignores PS|PS.*FALSE"
  )
})

test_that("build_causal_bundle errors on observational without X", {
  set.seed(1)
  y <- rnorm(20)
  T <- c(rep(0, 10), rep(1, 10))

  expect_error(
    build_causal_bundle(
      y = y,
      X = NULL,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      design = "observational",
      PS = "logit"
    ),
    regexp = "observational.*requires.*X"
  )
})

test_that("build_causal_bundle errors on observational without PS", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  expect_error(
    build_causal_bundle(
      y = y,
      X = X,
      T = T,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      J = 4,
      design = "observational",
      PS = FALSE
    ),
    regexp = "observational.*requires PS"
  )
})

# ======================================================================
# Bundle structure tests
# ======================================================================

test_that("build_causal_bundle returns correct class", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "rct"
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
})

test_that("build_causal_bundle has required components", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "rct"
  )

  expect_true("outcome" %in% names(bundle))
  expect_true("data" %in% names(bundle))
  expect_true("index" %in% names(bundle))
  expect_true("meta" %in% names(bundle))
})

test_that("build_causal_bundle creates outcome bundles for both arms", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "rct"
  )

  expect_true("trt" %in% names(bundle$outcome))
  expect_true("con" %in% names(bundle$outcome))
  expect_s3_class(bundle$outcome$trt, "dpmixgpd_bundle")
  expect_s3_class(bundle$outcome$con, "dpmixgpd_bundle")
})

test_that("build_causal_bundle stores correct indices", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "rct"
  )

  expect_equal(bundle$index$con, 1:10)
  expect_equal(bundle$index$trt, 11:20)
})

# ======================================================================
# Arm-specific parameter tests
# ======================================================================

test_that("build_causal_bundle allows arm-specific backends", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "crp"),  # trt=sb, con=crp
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "rct"
  )

  expect_equal(bundle$meta$backend$trt, "sb")
  expect_equal(bundle$meta$backend$con, "crp")
})

test_that("build_causal_bundle allows arm-specific kernels", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = c("normal", "gamma"),  # trt=normal, con=gamma
    GPD = FALSE,
    J = 4,
    design = "rct"
  )

  expect_equal(bundle$meta$kernel$trt, "normal")
  expect_equal(bundle$meta$kernel$con, "gamma")
})

test_that("build_causal_bundle allows arm-specific GPD", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = c(TRUE, FALSE),  # trt=TRUE, con=FALSE
    J = 4,
    design = "rct"
  )

  expect_true(bundle$meta$GPD$trt)
  expect_false(bundle$meta$GPD$con)
})

test_that("build_causal_bundle allows arm-specific J", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = c(6, 8),  # trt=6, con=8
    design = "rct"
  )

  expect_equal(bundle$meta$components$trt, 6)
  expect_equal(bundle$meta$components$con, 8)
})

test_that("build_causal_bundle allows arm-specific epsilon", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    epsilon = c(0.01, 0.05),  # trt=0.01, con=0.05
    design = "rct"
  )

  expect_equal(bundle$meta$epsilon$trt, 0.01)
  expect_equal(bundle$meta$epsilon$con, 0.05)
})

# ======================================================================
# PS model type tests
# ======================================================================

test_that("build_causal_bundle supports PS='logit'", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "observational",
    PS = "logit"
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_equal(bundle$meta$ps$model_type, "logit")
})

test_that("build_causal_bundle supports PS='probit'", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "observational",
    PS = "probit"
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_equal(bundle$meta$ps$model_type, "probit")
})

test_that("build_causal_bundle supports PS='naive'", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "observational",
    PS = "naive"
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_equal(bundle$meta$ps$model_type, "naive")
})

# ======================================================================
# Empty X handling tests
# ======================================================================

test_that("build_causal_bundle handles NULL X for RCT", {
  set.seed(1)
  y <- rnorm(20)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = NULL,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "rct"
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_false(bundle$meta$has_x)
})

test_that("build_causal_bundle handles empty matrix X", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(nrow = 20, ncol = 0)  # no columns
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    design = "rct"
  )

  expect_s3_class(bundle, "dpmixgpd_causal_bundle")
  expect_false(bundle$meta$has_x)
})

# ======================================================================
# MCMC settings tests
# ======================================================================

test_that("build_causal_bundle stores MCMC settings", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    mcmc_outcome = list(niter = 100, nburnin = 20, thin = 2, nchains = 1, seed = 42),
    design = "rct"
  )

  expect_equal(bundle$outcome$trt$mcmc$niter, 100)
  expect_equal(bundle$outcome$con$mcmc$niter, 100)
})

# ======================================================================
# param_specs tests
# ======================================================================

test_that("build_causal_bundle applies shared param_specs", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  param_specs <- list(
    bulk = list(
      mean = list(mode = "link", link = "identity")
    )
  )

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    param_specs = param_specs,
    design = "rct"
  )

  expect_equal(bundle$outcome$trt$spec$plan$bulk$mean$mode, "link")
  expect_equal(bundle$outcome$con$spec$plan$bulk$mean$mode, "link")
})

test_that("build_causal_bundle applies arm-specific param_specs", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  param_specs <- list(
    trt = list(bulk = list(mean = list(mode = "link", link = "identity"))),
    con = list(bulk = list(mean = list(mode = "dist")))
  )

  bundle <- build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    J = 4,
    param_specs = param_specs,
    design = "rct"
  )

  expect_equal(bundle$outcome$trt$spec$plan$bulk$mean$mode, "link")
  expect_equal(bundle$outcome$con$spec$plan$bulk$mean$mode, "dist")
})
