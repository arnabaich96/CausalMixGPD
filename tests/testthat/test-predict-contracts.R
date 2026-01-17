.fit_cache_env <- new.env(parent = emptyenv())

.get_cached_fit <- function(key, build_fun) {
  if (exists(key, envir = .fit_cache_env, inherits = FALSE)) {
    return(get(key, envir = .fit_cache_env, inherits = FALSE))
  }

  cache_key <- NULL
  cache_enabled <- if (exists(".cache_enabled", mode = "function")) {
    get(".cache_enabled", mode = "function")
  } else {
    NULL
  }
  cache_hash <- if (exists(".cache_hash", mode = "function")) {
    get(".cache_hash", mode = "function")
  } else {
    NULL
  }
  cache_get <- if (exists(".cache_get", mode = "function")) {
    get(".cache_get", mode = "function")
  } else {
    NULL
  }
  cache_set <- if (exists(".cache_set", mode = "function")) {
    get(".cache_set", mode = "function")
  } else {
    NULL
  }

  if (!is.null(cache_enabled) && isTRUE(cache_enabled()) && !is.null(cache_hash)) {
    cache_key <- cache_hash(key)
    cached <- if (!is.null(cache_get)) cache_get(cache_key) else NULL
    if (!is.null(cached) && inherits(cached$fit, "mixgpd_fit")) {
      assign(key, cached$fit, envir = .fit_cache_env)
      return(cached$fit)
    }
  }

  fit <- build_fun()
  assign(key, fit, envir = .fit_cache_env)

  if (!is.null(cache_key) && !is.null(cache_set)) {
    cache_set(cache_key, list(fit = fit))
  }

  fit
}

.build_uncond_fit <- function() {
  set.seed(101)
  y <- abs(stats::rnorm(40)) + 0.1

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "gamma",
    GPD = TRUE,
    components = 4,
    mcmc = list(niter = 120, nburnin = 20, thin = 1, nchains = 1, seed = 1)
  )

  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  res <- NULL
  utils::capture.output(
    res <- run_mcmc_bundle_manual(bundle, show_progress = FALSE),
    file = nullfile
  )

  res
}

.build_cond_fit <- function() {
  set.seed(202)
  y <- abs(stats::rnorm(40)) + 0.1
  X <- data.frame(x1 = stats::rnorm(40), x2 = stats::runif(40))

  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "gamma",
    GPD = FALSE,
    components = 4,
    mcmc = list(niter = 120, nburnin = 20, thin = 1, nchains = 1, seed = 2)
  )

  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  res <- NULL
  utils::capture.output(
    res <- run_mcmc_bundle_manual(bundle, show_progress = FALSE),
    file = nullfile
  )

  res
}

test_that("build_nimble_bundle validates key inputs", {

  set.seed(1)
  y <- abs(stats::rnorm(10)) + 0.1
  X_bad <- data.frame(x1 = stats::rnorm(9), x2 = stats::runif(9))

  expect_error(
    build_nimble_bundle(
      y = y,
      X = X_bad,
      backend = "sb",
      kernel = "gamma",
      GPD = FALSE,
      components = 4
    ),
    "same number of rows"
  )

  expect_error(
    build_nimble_bundle(
      y = y,
      backend = "bad",
      kernel = "gamma",
      GPD = FALSE,
      components = 4
    ),
    "one of"
  )

  expect_error(
    build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = "bad",
      GPD = FALSE,
      components = 4
    ),
    "one of"
  )

  expect_error(
    build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = "gamma",
      GPD = FALSE,
      components = 1
    ),
    "components must be an integer >= 2"
  )

  expect_error(
    build_nimble_bundle(
      y = y,
      backend = "sb",
      kernel = "cauchy",
      GPD = TRUE,
      components = 4
    ),
    "Cauchy kernels are never paired with GPD tails"
  )
})

test_that("predict() rejects invalid type and index inputs", {

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)

  expect_error(
    predict(fit, type = "bad-type"),
    "one of"
  )

  expect_error(
    predict(fit, type = "quantile", index = c(-0.1, 0.5)),
    "index must be in \\(0,1\\)"
  )
})

test_that("predict() preserves y-grid order", {

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)

  y_grid <- c(1.0, 0.2, 2.5, 0.5)
  pred <- predict(fit, type = "density", y = y_grid)

  expect_equal(pred$fit$y, y_grid)
})

test_that("predict() with newdata enforces column contracts", {

  fit <- .get_cached_fit("cond_fit_predict_contracts", .build_cond_fit)
  X_train <- fit$data$X
  X_new <- X_train[1:4, , drop = FALSE]

  X_missing <- X_new[, "x1", drop = FALSE]
  expect_error(
    predict(fit, x = X_missing, type = "mean"),
    "Column names of 'x' do not match training design matrix"
  )

  X_extra <- X_new
  X_extra$extra <- 1
  expect_error(
    predict(fit, x = X_extra, type = "mean"),
    "Column names of 'x' do not match training design matrix"
  )

  X_reorder <- X_new[, c("x2", "x1"), drop = FALSE]
  expect_silent(predict(fit, x = X_reorder, type = "mean"))

  X_single <- X_new[1, , drop = FALSE]
  pred_single <- predict(fit, x = X_single, y = c(0.3, 1.2), type = "density")
  expect_equal(nrow(pred_single$fit), 2)

  expect_error(
    predict(fit, x = c(0.1, 0.2), type = "mean"),
    "Number of columns in 'x' does not match training design matrix"
  )
})

test_that("CDF from survival is monotone and bounded", {

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)
  y_grid <- seq(0.05, 3.5, length.out = 40)
  pred_surv <- predict(fit, type = "survival", y = y_grid)

  cdf <- 1 - pred_surv$fit$survival
  expect_true(all(is.finite(cdf)))
  expect_true(all(cdf >= -1e-8 & cdf <= 1 + 1e-8))
  expect_true(all(diff(cdf) >= -1e-6))
})

test_that("GPD tail is approximately continuous at threshold", {

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)
  pr <- params(fit)
  u <- pr$threshold
  if (is.null(u) || !is.finite(u)) {
    skip("Threshold not available for this fit.")
  }

  eps <- max(1e-4, 0.01 * abs(u))
  y_grid <- c(u - eps, u + eps)
  pred_surv <- predict(fit, type = "survival", y = y_grid)
  cdf <- 1 - pred_surv$fit$survival

  expect_true(abs(cdf[2] - cdf[1]) < 0.05)
})

test_that("predict(type='sample') is reproducible with seed", {

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)

  set.seed(999)
  samp1 <- predict(fit, type = "sample", nsim = 50)$fit
  set.seed(999)
  samp2 <- predict(fit, type = "sample", nsim = 50)$fit
  set.seed(1000)
  samp3 <- predict(fit, type = "sample", nsim = 50)$fit

  expect_equal(samp1, samp2)
  expect_false(identical(samp1, samp3))
  expect_true(all(is.finite(samp1)))
  expect_true(all(samp1 > 0))
})

test_that("PIT residuals are finite and in [0,1]", {

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)
  res <- residuals(fit, type = "pit")

  expect_true(all(is.finite(res)))
  expect_true(all(res >= 0 & res <= 1))
})

test_that("ncores=1 and ncores=2 agree for deterministic predictions", {

  fit <- .get_cached_fit("cond_fit_predict_contracts", .build_cond_fit)
  X_new <- fit$data$X[1:3, , drop = FALSE]
  y_grid <- seq(0.1, 2.0, length.out = 8)

  p1 <- predict(fit, x = X_new, y = y_grid, type = "density", ncores = 1)
  p2 <- predict(fit, x = X_new, y = y_grid, type = "density", ncores = 2)

  expect_equal(p1$fit$density, p2$fit$density, tolerance = 1e-8)
})