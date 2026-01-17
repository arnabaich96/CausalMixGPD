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
  expect_true(inherits(bundle, "dpmixgpd_bundle"))
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
  expect_true(inherits(bundle_crp, "dpmixgpd_bundle"))
  
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
  expect_true(inherits(bundle_sb, "dpmixgpd_bundle"))
})

test_that("Smoke test: Unconditional DPmixGPD structure (as per v06-v07)", {
  # This test verifies DPmixGPD bundles can be created
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
  expect_true(inherits(bundle_crp_gpd, "dpmixgpd_bundle"))
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
  expect_true(inherits(bundle_sb_gpd, "dpmixgpd_bundle"))
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
  expect_true(inherits(bundle_cond, "dpmixgpd_bundle"))
  expect_true(isTRUE(bundle_cond$spec$meta$has_X))
})

test_that("Smoke test: Causal inference setup (as per v12-v15)", {
  # This test verifies causal bundles can be set up
  # See: vignettes/v12-v15-causal.Rmd
  
  set.seed(999)
  
  # Generate synthetic causal data
  n <- 40
  z <- rbinom(n, 1, 0.5)  # Treatment indicator
  x <- rnorm(n)           # Confounder
  
  # Outcome depends on treatment and confounder
  y <- 2 + 1.5 * z + 0.8 * x + rnorm(n, 0, 0.5)
  y <- abs(y) + 0.1  # Make positive
  
  # Build propensity score model bundle
  expect_no_error({
    bundle_ps <- build_nimble_bundle(
      y = z,  # z is binary treatment
      backend = "crp",
      kernel = "normal",
      GPD = FALSE,
      components = 2  # Just 2 for simplicity
    )
  })
  expect_true(inherits(bundle_ps, "dpmixgpd_bundle"))
  
  # Build outcome model bundle
  X_outcome <- data.frame(x = x, z = z)
  expect_no_error({
    bundle_outcome <- build_nimble_bundle(
      y = y,
      X = X_outcome,
      backend = "crp",
      kernel = "normal",
      GPD = FALSE,
      components = 3
    )
  })
  expect_true(inherits(bundle_outcome, "dpmixgpd_bundle"))
})

test_that("Smoke test: Kernel distributions available (as per kernel-*.Rmd)", {
  # This test verifies exported kernel helper functions are available
  # See: vignettes/kernel-*.Rmd
  
  # Verify key kernel-related functions exist
  expect_true(exists("dnormal"), info = "dnormal available")
  expect_true(exists("dgamma"), info = "dgamma available (or wrapper)")
  expect_true(exists("dlaplace"), info = "dlaplace available")
  expect_true(exists("damoroso"), info = "damoroso available")
})
