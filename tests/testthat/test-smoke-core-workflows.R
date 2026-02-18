test_that("Smoke test: Basic workflow (as per v03)", {
  # This test verifies the basic user workflow: bundle → compile → run MCMC
  # See: vignettes/v03-basic-model-compile-run.Rmd

  set.seed(42)

  # Generate sample data
  y <- abs(rnorm(30)) + 0.1

  # Step 1: Build bundle (should succeed silently)
  expect_no_error({
    bundle <- build_nimble_bundle(
      y = y,
      backend = "crp",
      kernel = "normal",
      GPD = FALSE,
      components = 4
    )
  })

  # Step 2: Verify bundle has required structure
  expect_true(inherits(bundle, "causalmixgpd_bundle"))
  expect_true(is.list(bundle$code))
  expect_true(is.list(bundle$spec))

  # Note: Full MCMC sampling is too slow for smoke tests,
  # so we skip the compile/run step. The vignettes handle the actual workflow.
})

test_that("Smoke test: Unconditional DPmix structure (as per v04-v07)", {
  # This test verifies bundles can be created for both backends
  # See: vignettes/v04-v07-unconditional.Rmd

  set.seed(123)
  y <- abs(rnorm(20)) + 0.1

  # Test CRP backend
  expect_no_error({
    bundle_crp <- build_nimble_bundle(
      y = y,
      backend = "crp",
      kernel = "normal",
      GPD = FALSE,
      components = 3
    )
  })
  expect_true(inherits(bundle_crp, "causalmixgpd_bundle"))

  # Test SB backend
  expect_no_error({
    bundle_sb <- build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = "normal",
      GPD = FALSE,
      components = 3
    )
  })
  expect_true(inherits(bundle_sb, "causalmixgpd_bundle"))
})

test_that("Smoke test: Unconditional CausalMixGPD structure (as per v06-v07)", {
  # This test verifies CausalMixGPD bundles can be created
  # See: vignettes/v06-v07-unconditional-GPD.Rmd

  set.seed(789)
  y <- c(abs(rnorm(15)), 10 + abs(rnorm(5)))  # Mix with some tail values

  # Test CRP backend with GPD
  expect_no_error({
    bundle_crp_gpd <- build_nimble_bundle(
      y = y,
      backend = "crp",
      kernel = "normal",
      GPD = TRUE,
      components = 3
    )
  })
  expect_true(inherits(bundle_crp_gpd, "causalmixgpd_bundle"))
  expect_true(isTRUE(bundle_crp_gpd$spec$meta$GPD))

  # Test SB backend with GPD
  expect_no_error({
    bundle_sb_gpd <- build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = "normal",
      GPD = TRUE,
      components = 3
    )
  })
  expect_true(inherits(bundle_sb_gpd, "causalmixgpd_bundle"))
  expect_true(isTRUE(bundle_sb_gpd$spec$meta$GPD))
})

test_that("Smoke test: Conditional model structure (as per v08-v11)", {
  # This test verifies conditional bundles (with covariates) can be created
  # See: vignettes/v08-v11-conditional.Rmd

  set.seed(555)
  y <- abs(rnorm(25)) + 0.1
  X <- data.frame(
    x1 = rnorm(25),
    x2 = runif(25)
  )

  # Test conditional DPmix
  expect_no_error({
    bundle_cond <- build_nimble_bundle(
      y = y,
      X = X,
      backend = "crp",
      kernel = "normal",
      GPD = FALSE,
      components = 3
    )
  })
  expect_true(inherits(bundle_cond, "causalmixgpd_bundle"))
  expect_true(isTRUE(bundle_cond$spec$meta$has_X))
})

test_that("Smoke test: Causal bundle setup (build_causal_bundle API)", {
  # This test verifies the actual causal API bundle creation
  # Note: MCMC fitting is tested in test-causal.R

  set.seed(999)


  # Generate synthetic causal data
  n <- 30
  X <- data.frame(
    x1 = rnorm(n),
    x2 = runif(n, -1, 1)
  )
  T_ind <- rbinom(n, 1, plogis(0.2 + 0.5 * X$x1))  # Treatment indicator

  # Outcome depends on treatment and confounders
  y <- 2 + 1.5 * T_ind + 0.8 * X$x1 + 0.3 * X$x2 + rnorm(n, 0, 0.5)
  y <- abs(y) + 0.1  # Make positive

  # Test observational design with PS model
  expect_no_error({
    causal_bundle <- build_causal_bundle(
      y = y,
      X = X,
      A = T_ind,
      backend = "crp",
      kernel = "normal",
      GPD = FALSE,
      components = 3,
      PS = "logit",

    )
  })
  expect_true(inherits(causal_bundle, "causalmixgpd_causal_bundle"))
  expect_true(causal_bundle$meta$ps$enabled)
  expect_equal(causal_bundle$meta$ps$model_type, "logit")

  # Test RCT design without PS model
  expect_no_error({
    causal_bundle_rct <- build_causal_bundle(
      y = y,
      X = X,
      A = T_ind,
      backend = "sb",
      kernel = "gamma",
      GPD = FALSE,
      components = 3,
      PS = FALSE,

    )
  })
  expect_true(inherits(causal_bundle_rct, "causalmixgpd_causal_bundle"))
  expect_false(causal_bundle_rct$meta$ps$enabled)
})

test_that("Smoke test: Kernel registry and distributions available", {
  # This test verifies the kernel registry is initialized and key functions exist

  # Verify kernel registry is available
  registry <- get_kernel_registry()
  expect_true(is.list(registry))
  expect_true(length(registry) > 0)

  # Check expected kernels are registered
  expected_kernels <- c("normal", "gamma", "lognormal", "laplace", "invgauss", "amoroso", "cauchy")
  for (k in expected_kernels) {
    expect_true(k %in% names(registry), info = paste0("kernel '", k, "' registered"))
  }

  # Verify key exported distribution functions exist
  expect_true(exists("dNormMix", mode = "function"), info = "dNormMix available")
  expect_true(exists("dGammaMix", mode = "function"), info = "dGammaMix available")
  expect_true(exists("dLaplaceMix", mode = "function"), info = "dLaplaceMix available")
  expect_true(exists("dAmoroso", mode = "function"), info = "dAmoroso available")
  expect_true(exists("dGpd", mode = "function"), info = "dGpd available")
})

test_that("Smoke test: Spliced backend bundle creation", {
  # Test that spliced backend bundles can be created for various mode combinations
  
  set.seed(456)
  y <- abs(rnorm(25)) + 0.1
  X <- matrix(rnorm(25 * 2), ncol = 2)
  
  # Test 1: Constant model (X=NULL) with fixed/dist modes
  expect_no_error({
    bundle_const <- build_nimble_bundle(
      y = y,
      backend = "spliced",
      kernel = "gamma",
      GPD = TRUE,
      components = 3,
      param_specs = list(
        gpd = list(
          threshold = list(mode = "fixed", value = 0.5),
          tail_scale = list(mode = "dist", dist = "invgamma", args = list(shape = 2, scale = 1)),
          tail_shape = list(mode = "fixed", value = 0.1)
        )
      )
    )
  })
  expect_true(inherits(bundle_const, "causalmixgpd_bundle"))
  expect_equal(bundle_const$spec$meta$backend, "spliced")
  
  # Test 2: Covariate model with link mode
  expect_no_error({
    bundle_link <- build_nimble_bundle(
      y = y,
      X = X,
      backend = "spliced",
      kernel = "normal",
      GPD = TRUE,
      components = 3,
      param_specs = list(
        gpd = list(
          threshold = list(mode = "link", link = "exp"),
          tail_scale = list(mode = "dist", dist = "invgamma", args = list(shape = 3, scale = 1)),
          tail_shape = list(mode = "link", link = "identity")
        )
      )
    )
  })
  expect_true(inherits(bundle_link, "causalmixgpd_bundle"))
  expect_equal(bundle_link$spec$meta$backend, "spliced")
  
  # Test 3: Mixed modes
  expect_no_error({
    bundle_mixed <- build_nimble_bundle(
      y = y,
      X = X,
      backend = "spliced",
      kernel = "lognormal",
      GPD = TRUE,
      components = 2,
      param_specs = list(
        gpd = list(
          threshold = list(mode = "link", link = "exp"),
          tail_scale = list(mode = "fixed", value = 1.0),
          tail_shape = list(mode = "dist", dist = "normal", args = list(mean = 0.2, sd = 0.1))
        )
      )
    )
  })
  expect_true(inherits(bundle_mixed, "causalmixgpd_bundle"))
})
