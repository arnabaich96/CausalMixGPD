# test-fitted.R
# Tests for fitted.mixgpd_fit, params(), and plot.mixgpd_fitted
#
# Tier B (ci): Requires MCMC fit

if (!exists(".cache_enabled")) {
  helper_path <- file.path("tests", "testthat", "helper-cache.R")
  if (file.exists(helper_path)) source(helper_path)
}

set.seed(123)
N <- 25
y <- abs(rnorm(N)) + 0.1
X <- data.frame(x1 = rnorm(N), x2 = runif(N))

mcmc_cfg <- list(niter = 60, nburnin = 20, thin = 1, nchains = 1, seed = 1)
cache_key <- NULL
if (exists(".cache_enabled") && isTRUE(.cache_enabled())) {
  key_str <- paste("fitted", "conditional", N, mcmc_cfg$niter, mcmc_cfg$nburnin,
                   mcmc_cfg$thin, mcmc_cfg$nchains, mcmc_cfg$seed, sep = "|")
  cache_key <- .cache_hash(key_str)
}
cached <- if (!is.null(cache_key)) .cache_get(cache_key) else NULL

if (!is.null(cached) && inherits(cached$fit, "mixgpd_fit")) {
  fit <- cached$fit
} else {
  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 5,
    mcmc = mcmc_cfg
  )
  fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
  if (!is.null(cache_key)) .cache_set(cache_key, list(fit = fit))
}

test_that("Conditional fitted returns one value per observation", {
  skip_if_not_test_level("ci")
  
  ftd <- fitted(fit, type = "median", level = 0.9, seed = 1)
  expect_s3_class(ftd, "mixgpd_fitted")
  expect_equal(nrow(ftd), N)
  expect_true(all(is.finite(ftd$fit)))
})

test_that("Conditional fitted supports quantile type", {
  skip_if_not_test_level("ci")
  
  ftd <- fitted(fit, type = "quantile", p = 0.8, level = 0.9, seed = 1)
  expect_equal(nrow(ftd), N)
  expect_true(all(is.finite(ftd$fit)))
})

# =============================================================================
# Tests for params() extractor
# =============================================================================
test_that("params() returns mixgpd_params object with expected structure", {
  skip_if_not_test_level("ci")
  
  p <- params(fit)
  
  expect_s3_class(p, "mixgpd_params")
  expect_true(is.list(p))
  
  # Should have alpha (concentration parameter)
  expect_true("alpha" %in% names(p))
  expect_true(is.numeric(p$alpha))
  expect_true(is.finite(p$alpha))
  
  # Should have weights
  expect_true("w" %in% names(p))
  expect_true(is.numeric(p$w))
  expect_true(all(p$w >= 0))
  expect_true(abs(sum(p$w) - 1) < 0.1)  # Weights should sum to ~1
})

test_that("params() print method works", {
  skip_if_not_test_level("ci")
  
  p <- params(fit)
  
  expect_output(print(p), "Posterior mean parameters")
  expect_output(print(p), "alpha")
})

# =============================================================================
# Tests for plot.mixgpd_fitted
# =============================================================================
test_that("plot.mixgpd_fitted returns diagnostic plots", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")
  
  ftd <- fitted(fit, type = "mean", level = 0.9, seed = 1)
  
  expect_no_error({
    plots <- plot(ftd)
  })
  
  expect_s3_class(plots, "mixgpd_fitted_plots")
  expect_true(is.list(plots))
  expect_true("observed_fitted_plot" %in% names(plots))
  expect_true("residual_plot" %in% names(plots))
  
  # Both should be ggplot objects
  expect_true(inherits(plots$observed_fitted_plot, "ggplot"))
  expect_true(inherits(plots$residual_plot, "ggplot"))
})
