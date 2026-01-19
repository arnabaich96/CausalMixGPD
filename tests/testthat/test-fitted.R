# Tests for fitted.mixgpd_fit behavior


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
  ftd <- fitted(fit, type = "median", level = 0.9, seed = 1)
  expect_s3_class(ftd, "mixgpd_fitted")
  expect_equal(nrow(ftd), N)
  expect_true(all(is.finite(ftd$fit)))
  expect_true(all(c("obs", "fit", "lower", "upper", "residuals") %in% names(ftd)))
})
