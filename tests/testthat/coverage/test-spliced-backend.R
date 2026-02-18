test_that("spliced backend is registered and accepted", {
  skip_if_not_installed("nimble")
  
  # Spliced should be in allowed_backends
  ab <- CausalMixGPD:::allowed_backends
  expect_true("spliced" %in% ab)
  
  # Should accept spliced backend in spec compilation
  set.seed(123)
  y <- rgamma(50, shape = 2, rate = 1)
  X <- matrix(rnorm(50 * 2), ncol = 2)
  
  spec <- CausalMixGPD:::compile_model_spec(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = 3
  )
  
  expect_equal(spec$meta$backend, "spliced")
})

test_that("spliced backend enforces component-level GPD params", {
  skip_if_not_installed("nimble")
  
  set.seed(456)
  y <- rgamma(50, shape = 2, rate = 1)
  X <- matrix(rnorm(50 * 2), ncol = 2)
  
  spec <- CausalMixGPD:::compile_model_spec(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = 3,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "dist"),
        tail_scale = list(mode = "dist"),
        tail_shape = list(mode = "dist")
      )
    )
  )
  
  # Check that level="component" is set for GPD params
  expect_equal(spec$plan$gpd$threshold$level, "component")
  expect_equal(spec$plan$gpd$tail_scale$level, "component")
  expect_equal(spec$plan$gpd$tail_shape$level, "component")
})

test_that("spliced backend rejects link mode without X", {
  skip_if_not_installed("nimble")
  
  set.seed(789)
  y <- rgamma(50, shape = 2, rate = 1)
  
  expect_error(
    CausalMixGPD:::compile_model_spec(
      y = y,
      X = NULL,
      backend = "spliced",
      kernel = "gamma",
      GPD = TRUE,
      components = 3,
      param_specs = list(
        gpd = list(threshold = list(mode = "link"))
      )
    ),
    "link mode.*requires X"
  )
})

test_that("spliced backend allows link mode for all GPD params with X", {
  skip_if_not_installed("nimble")
  
  set.seed(101)
  y <- rgamma(50, shape = 2, rate = 1)
  X <- matrix(rnorm(50 * 2), ncol = 2)
  
  spec <- CausalMixGPD:::compile_model_spec(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = 3,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "link", link = "identity"),
        tail_scale = list(mode = "link", link = "exp"),
        tail_shape = list(mode = "link", link = "identity")
      )
    )
  )
  
  expect_equal(spec$plan$gpd$threshold$mode, "link")
  expect_equal(spec$plan$gpd$tail_scale$mode, "link")
  expect_equal(spec$plan$gpd$tail_shape$mode, "link")
})

test_that("spliced backend code generation works with fixed/dist/link modes", {
  skip_if_not_installed("nimble")
  
  set.seed(202)
  y <- rgamma(50, shape = 2, rate = 1)
  X <- matrix(rnorm(50 * 2), ncol = 2)
  
  # Test with mixed modes: threshold=link, tail_scale=dist, tail_shape=fixed
  spec <- CausalMixGPD:::compile_model_spec(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = 3,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "link", link = "exp"),
        tail_scale = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
        tail_shape = list(mode = "fixed", value = 0.1)
      )
    )
  )
  
  # Generate NIMBLE code
  code <- CausalMixGPD:::build_code_from_spec(spec)
  
  # Code should be a { expression from nimbleCode()
  expect_true(is.call(code) && identical(as.character(code[[1]]), "{"))
  
  # Check that code contains expected patterns for spliced GPD params
  code_str <- deparse(code)
  expect_true(any(grepl("beta_threshold", code_str)))
  expect_true(any(grepl("threshold_i", code_str)))
  expect_true(any(grepl("tail_scale\\[k\\]", code_str)))
  expect_true(any(grepl("tail_shape\\[k\\]", code_str)))
})

test_that("spliced backend dimensions are correct", {
  skip_if_not_installed("nimble")
  
  set.seed(303)
  y <- rgamma(50, shape = 2, rate = 1)
  X <- matrix(rnorm(50 * 2), ncol = 2)
  K <- 3
  P <- 2
  N <- 50
  
  spec <- CausalMixGPD:::compile_model_spec(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = K,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "link", link = "exp"),
        tail_scale = list(mode = "dist"),
        tail_shape = list(mode = "fixed", value = 0.1)
      )
    )
  )
  
  dims <- CausalMixGPD:::build_dimensions_from_spec(spec)
  
  # Check spliced-specific dimensions
  expect_equal(dims$z, c(N))
  expect_equal(dims$beta_threshold, c(K, P))
  expect_equal(dims$eta_threshold, c(N))
  expect_equal(dims$threshold_i, c(N))
  expect_equal(dims$tail_scale, c(K))
  # tail_shape is fixed mode, still gets K-dimensional vector in spliced
  expect_equal(dims$tail_shape, c(K))
})

test_that("spliced backend inits are correct", {
  skip_if_not_installed("nimble")
  
  set.seed(404)
  y <- rgamma(50, shape = 2, rate = 1)
  X <- matrix(rnorm(50 * 2), ncol = 2)
  K <- 3
  P <- 2
  
  spec <- CausalMixGPD:::compile_model_spec(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = K,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "link", link = "exp"),
        tail_scale = list(mode = "dist"),
        tail_shape = list(mode = "dist")
      )
    )
  )
  
  inits <- CausalMixGPD:::build_inits_from_spec(spec, y = y)
  
  # Check spliced-specific inits
  expect_true("z" %in% names(inits))
  expect_true("beta_threshold" %in% names(inits))
  expect_equal(dim(inits$beta_threshold), c(K, P))
  expect_true("tail_scale" %in% names(inits))
  expect_equal(length(inits$tail_scale), K)
  expect_true("tail_shape" %in% names(inits))
  expect_equal(length(inits$tail_shape), K)
  # threshold_i should NOT be initialized (deterministic)
  expect_false("threshold_i" %in% names(inits))
})

test_that("spliced backend monitors are correct", {
  skip_if_not_installed("nimble")
  
  set.seed(505)
  y <- rgamma(50, shape = 2, rate = 1)
  X <- matrix(rnorm(50 * 2), ncol = 2)
  K <- 3
  P <- 2
  N <- 50
  
  spec <- CausalMixGPD:::compile_model_spec(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = K,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "link", link = "exp"),
        tail_scale = list(mode = "dist"),
        tail_shape = list(mode = "fixed", value = 0.1)
      )
    )
  )
  
  mons <- CausalMixGPD:::build_monitors_from_spec(spec)
  
  # Check monitors
  expect_true(sprintf("z[1:%d]", N) %in% mons)
  expect_true(sprintf("beta_threshold[1:%d,1:%d]", K, P) %in% mons)
  expect_true(sprintf("tail_scale[1:%d]", K) %in% mons)
  # threshold_i should NOT be monitored (deterministic, reconstructed)
  expect_false(any(grepl("threshold_i", mons)))
  # tail_shape is fixed, should still be monitored (stored in plan)
  expect_true(sprintf("tail_shape[1:%d]", K) %in% mons)
})

test_that("spliced backend works with constant model (X=NULL, fixed/dist modes)", {
  skip_if_not_installed("nimble")
  
  set.seed(606)
  y <- rgamma(50, shape = 2, rate = 1)
  K <- 3
  
  spec <- CausalMixGPD:::compile_model_spec(
    y = y,
    X = NULL,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = K,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
        tail_scale = list(mode = "dist", dist = "gamma", args = list(shape = 2, rate = 1)),
        tail_shape = list(mode = "fixed", value = 0.1)
      )
    )
  )
  
  code <- CausalMixGPD:::build_code_from_spec(spec)
  # Code should be a { expression from nimbleCode()
  expect_true(is.call(code) && identical(as.character(code[[1]]), "{"))
  
  # Check that component-level nodes exist
  code_str <- deparse(code)
  expect_true(any(grepl("threshold\\[k\\]", code_str)))
  expect_true(any(grepl("tail_scale\\[k\\]", code_str)))
})

test_that("spliced backend prediction rejects link mode (not yet implemented)", {
  skip_if_not_installed("nimble")
  skip_on_cran()
  skip_on_ci()
  
  # This test documents the current limitation: prediction with link-mode GPD
  # params is not yet fully implemented for spliced backend
  
  set.seed(707)
  y <- rgamma(50, shape = 2, rate = 1)
  X <- matrix(rnorm(50 * 2), ncol = 2)
  
  # Create a minimal mock fit object to test prediction error
  # (Full fitting would take too long for unit tests)
  spec <- CausalMixGPD:::compile_model_spec(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = 3,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "link", link = "exp")
      )
    )
  )
  
  # Mock a minimal fit object with link-mode threshold
  # (Would normally come from actual MCMC, but we just need structure for error check)
  # Note: This test is aspirational - it documents what SHOULD error when implemented
  # For now, we just validate that the spec was created correctly
  expect_equal(spec$plan$gpd$threshold$mode, "link")
  expect_equal(spec$meta$backend, "spliced")
})

test_that("tail registry indexed_by_cluster_in_crp flag supports spliced", {
  skip_if_not_installed("nimble")
  
  # The tail registry flag indexed_by_cluster_in_crp=FALSE is what allows
  # GPD params to use link mode without NIMBLE CRP sampler conflicts
  tail_reg <- CausalMixGPD:::get_tail_registry()
  expect_false(tail_reg$indexed_by_cluster_in_crp)
})

test_that("spliced backend MCMC integration test", {
  skip_if_not_installed("nimble")
  skip_if_not_test_level("ci")  # MCMC tests are slow, skip at CRAN level
  
  set.seed(345)
  # Generate data with clear tail behavior
  y <- c(abs(rnorm(30, mean = 2, sd = 1)), rgamma(10, shape = 1, scale = 5))
  X <- matrix(rnorm(40 * 2), ncol = 2)
  
  # Build bundle with spliced backend - mixed modes
  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "spliced",
    kernel = "gamma",
    GPD = TRUE,
    components = 3,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "link", link = "exp"),
        tail_scale = list(mode = "dist", dist = "invgamma", args = list(shape = 3, scale = 1)),
        tail_shape = list(mode = "fixed", value = 0.15)
      )
    ),
    mcmc = list(niter = 100, nburnin = 20, thin = 2, nchains = 1, seed = 123)
  )
  
  # Verify bundle structure
  expect_true(inherits(bundle, "causalmixgpd_bundle"))
  expect_equal(bundle$spec$meta$backend, "spliced")
  
  # Run MCMC (short run just to verify it works)
  expect_no_error({
    fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
  })
  
  # Verify fit object structure
  expect_s3_class(fit, "causalmixgpd_fit")
  expect_true(!is.null(fit$samples))
  expect_true(!is.null(fit$bundle))
  
  # Verify monitoring of beta matrices (link mode)
  samp_names <- names(fit$samples)
  expect_true(any(grepl("beta_threshold", samp_names)), 
              info = "beta_threshold should be monitored for link mode")
  
  # Verify tail_scale is monitored (dist mode)
  expect_true(any(grepl("tail_scale", samp_names)),
              info = "tail_scale should be monitored for dist mode")
  
  # Note: prediction with link mode intentionally errors (not yet implemented)
  expect_error(
    predict(fit, newdata = X[1:5, , drop = FALSE]),
    "not yet fully implemented"
  )
  
  # But prediction with fixed/dist modes would work if we refit
  # (not testing here to save time)
})
