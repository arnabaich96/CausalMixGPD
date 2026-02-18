# test-predict.R
# Consolidated prediction tests: contracts, unconditional, quantile branches
#
# Tier A (cran): Input validation (build_nimble_bundle)
# Tier B (ci):   Prediction behavior requiring MCMC fits

if (!exists(".cache_enabled")) {
  helper_path <- file.path("tests", "testthat", "helper-02-cache.R")
  if (file.exists(helper_path)) source(helper_path)
}

# =============================================================================
# Helpers from test-predict-contracts.R
# =============================================================================
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

# =============================================================================
# Helpers from test-predict-unconditional.R
# =============================================================================
.fit_uncond_legacy_env <- new.env(parent = emptyenv())
.fit_uncond_legacy_env$fit <- NULL

.build_unconditional_fit <- function() {
  set.seed(42)
  y <- abs(rnorm(60)) + 0.2

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "gamma",
    GPD = TRUE,
    components = 8,
    mcmc = list(niter = 300, nburnin = 50, nchains = 1)
  )

  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  res <- NULL
  utils::capture.output(
    res <- run_mcmc_bundle_manual(bundle, show_progress = FALSE),
    file = nullfile
  )
  res
}

.get_fit_uncond <- function() {
  if (is.null(.fit_uncond_legacy_env$fit)) {
    .fit_uncond_legacy_env$fit <- .build_unconditional_fit()
  }
  .fit_uncond_legacy_env$fit
}

# =============================================================================
# Prediction contract tests (from test-predict-contracts.R)
# =============================================================================
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
  skip_if_not_test_level("ci")

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
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)

  y_grid <- c(1.0, 0.2, 2.5, 0.5)
  pred <- predict(fit, type = "density", y = y_grid)

  expect_equal(pred$fit$y, y_grid)
})

test_that("predict() with newdata enforces column contracts", {
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("cond_fit_predict_contracts", .build_cond_fit)
  X_train <- fit$data$X
  X_new <- X_train[1:4, , drop = FALSE]

  X_missing <- X_new[, "x1", drop = FALSE]
  expect_error(
    predict(fit, x = X_missing, type = "mean"),
    "Column names|Number of columns"
  )

  X_extra <- X_new
  X_extra$extra <- 1
  expect_error(
    predict(fit, x = X_extra, type = "mean"),
    "Column names|Number of columns"
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
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)
  y_grid <- seq(0.05, 3.5, length.out = 40)
  pred_surv <- predict(fit, type = "survival", y = y_grid)

  cdf <- 1 - pred_surv$fit$survival
  expect_true(all(is.finite(cdf)))
  expect_true(all(cdf >= -1e-8 & cdf <= 1 + 1e-8))
  expect_true(all(diff(cdf) >= -1e-6))
})

test_that("GPD tail is approximately continuous at threshold", {
  skip_if_not_test_level("ci")

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
  skip_if_not_test_level("ci")

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

test_that("PIT residuals are finite and in [0,1] (conditional fit)", {
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("cond_fit_predict_contracts", .build_cond_fit)
  res <- residuals(fit, type = "pit", pit = "plugin")
  expect_true(all(is.finite(res)))
  expect_true(all(res >= 0 & res <= 1))
  expect_equal(attr(res, "pit_type"), "plugin")
})

test_that("PIT modes (plugin, bayes_mean, bayes_draw) return [0,1] and pit_type attribute", {
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("cond_fit_predict_contracts", .build_cond_fit)
  N <- nrow(fit$data$X)

  for (pit_mode in c("plugin", "bayes_mean", "bayes_draw")) {
    res <- residuals(fit, type = "pit", pit = pit_mode, pit_seed = 1L)
    expect_true(all(res >= 0 & res <= 1), info = paste0("pit=", pit_mode))
    expect_true(all(is.finite(res)), info = paste0("pit=", pit_mode))
    expect_equal(attr(res, "pit_type"), pit_mode, info = paste0("pit=", pit_mode))
    expect_equal(length(res), N, info = paste0("pit=", pit_mode))
  }
})

test_that("ncores=1 and ncores=2 agree for deterministic predictions", {
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("cond_fit_predict_contracts", .build_cond_fit)
  X_new <- fit$data$X[1:3, , drop = FALSE]
  y_grid <- seq(0.1, 2.0, length.out = 8)

  p1 <- predict(fit, x = X_new, y = y_grid, type = "density", ncores = 1)
  p2 <- predict(fit, x = X_new, y = y_grid, type = "density", ncores = 2)

  expect_equal(p1$fit$density, p2$fit$density, tolerance = 1e-8)
})

test_that("quantile estimates average q_fun draws and median matches quantile(0.5)", {
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)

  pred_q <- predict(fit, type = "quantile", index = c(0.25, 0.5, 0.75),
                    interval = "none", store_draws = TRUE)
  expect_true(is.matrix(pred_q$draws))
  expect_equal(pred_q$fit$estimate, rowMeans(pred_q$draws, na.rm = TRUE), tolerance = 1e-8)

  pred_med <- predict(fit, type = "median", interval = "none", store_draws = TRUE)
  pred_q50 <- predict(fit, type = "quantile", index = 0.5, interval = "none", store_draws = TRUE)
  expect_equal(pred_med$fit$estimate, pred_q50$fit$estimate, tolerance = 1e-8)
})

# -----------------------------------------------------------------------------
# Mean vs restricted mean when GPD tail_shape (xi) >= 1
# -----------------------------------------------------------------------------
test_that("predict(type='mean') returns Inf when posterior has tail_shape >= 1", {
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)
  smp_orig <- fit$mcmc$samples %||% fit$samples
  if (is.null(smp_orig) || !("tail_shape" %in% colnames(as.matrix(smp_orig[[1]])))) {
    skip("Fit has no tail_shape (non-GPD or unexpected structure)")
  }
  # Copy samples so we do not mutate the cached fit
  smp <- lapply(smp_orig, function(ch) {
    m <- as.matrix(ch)
    coda::as.mcmc(m)
  })
  ch <- as.matrix(smp[[1]])
  ch[1L, "tail_shape"] <- 1.5
  smp[[1]] <- coda::as.mcmc(ch)
  fit_patched <- fit
  if (!is.null(fit_patched$mcmc$samples)) fit_patched$mcmc$samples <- smp
  if (!is.null(fit_patched$samples)) fit_patched$samples <- smp

  expect_warning(
    pred_mean <- predict(fit_patched, type = "mean", interval = "none", store_draws = FALSE),
    "infinite"
  )
  expect_equal(pred_mean$fit$estimate, Inf)
})

test_that("predict(type='rmean', cutoff=...) returns finite estimates", {
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)
  pred_rmean <- predict(fit, type = "rmean", cutoff = 50, interval = "none", store_draws = FALSE)
  expect_true(all(is.finite(pred_rmean$fit$estimate)))
  expect_true(is.numeric(pred_rmean$fit$estimate))
})

# -----------------------------------------------------------------------------
# PIT residuals: [0,1], length, pit_type (plugin, bayes_mean, bayes_draw)
# -----------------------------------------------------------------------------
# (PIT tests above already cover all modes and conditional length.)

test_that("check_glue_validity passes for known-good fit", {
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)
  out <- check_glue_validity(fit, n_draws = 20L, check_continuity = TRUE)
  expect_true(is.list(out))
  expect_true(is.list(out$pass))
  expect_true(isTRUE(out$pass$cdf_range))
  expect_true(isTRUE(out$pass$cdf_monotone))
  expect_true(isTRUE(out$pass$density_nonneg))
})

test_that("check_glue_validity CI-safe smoke test (n_draws=5, short grid)", {
  testthat::skip_on_cran()
  skip_if_not_test_level("ci")

  fit <- .get_cached_fit("uncond_fit_predict_contracts", .build_uncond_fit)
  small_grid <- seq(0.1, 5, length.out = 20)
  out <- check_glue_validity(fit, n_draws = 5L, grid = small_grid, check_continuity = FALSE)
  expect_true(is.list(out))
  expect_true(is.list(out$pass))
  expect_true(isTRUE(out$pass$cdf_range))
  expect_true(isTRUE(out$pass$cdf_monotone))
  expect_true(isTRUE(out$pass$density_nonneg))
})

# =============================================================================
# Unconditional predict tests (from test-predict-unconditional.R)
# =============================================================================
test_that("Unconditional model fitted successfully", {
  skip_if_not_test_level("ci")

  fit <- .get_fit_uncond()
  expect_s3_class(fit, "mixgpd_fit")
  expect_true(!is.null(fit$samples))
})

test_that("Quantile prediction with default quartiles", {
  skip_if_not_test_level("ci")

  fit <- .get_fit_uncond()
  pred_quant <- predict(fit, type = "quantile", interval = "credible")

  expect_s3_class(pred_quant, "mixgpd_predict")
  expect_type(pred_quant, "list")
  expect_equal(pred_quant$type, "quantile")
  expect_s3_class(pred_quant$fit, "data.frame")
  expect_true(all(c("index", "estimate", "lower", "upper") %in% names(pred_quant$fit)))
  expect_equal(nrow(pred_quant$fit), 3)
  expect_true(all(is.finite(pred_quant$fit$estimate)))
  expect_true(all(pred_quant$fit$lower <= pred_quant$fit$estimate))
  expect_true(all(pred_quant$fit$estimate <= pred_quant$fit$upper))
  expect_silent(p <- plot(pred_quant))
})

test_that("Quantile prediction with custom index", {
  skip_if_not_test_level("ci")

  fit <- .get_fit_uncond()
  custom_idx <- c(0.1, 0.5, 0.9)
  pred_quant <- predict(fit, type = "quantile", index = custom_idx, interval = "credible")

  expect_equal(nrow(pred_quant$fit), length(custom_idx))
  expect_equal(pred_quant$fit$index, custom_idx)
  expect_silent(p <- plot(pred_quant))
})

test_that("Mean prediction returns data frame format", {
  skip_if_not_test_level("ci")

  fit <- .get_fit_uncond()
  pred_mean <- predict(fit, type = "mean", interval = "credible", nsim_mean = 200)

  expect_s3_class(pred_mean, "mixgpd_predict")
  expect_equal(pred_mean$type, "mean")
  expect_s3_class(pred_mean$fit, "data.frame")
  expect_true(all(c("estimate", "lower", "upper") %in% names(pred_mean$fit)))
  expect_equal(nrow(pred_mean$fit), 1)
  expect_true(!is.null(pred_mean$draws))
  expect_true(length(pred_mean$draws) > 0)
  expect_type(pred_mean$draws, "double")
  expect_true(pred_mean$fit$lower <= pred_mean$fit$estimate)
  expect_true(pred_mean$fit$estimate <= pred_mean$fit$upper)
  expect_silent(p <- plot(pred_mean))
})

test_that("Mean prediction with custom credible level", {
  skip_if_not_test_level("ci")

  fit <- .get_fit_uncond()
  pred_mean_90 <- predict(fit, type = "mean", interval = "credible",
                          level = 0.90, nsim_mean = 200)
  pred_mean_99 <- predict(fit, type = "mean", interval = "credible",
                          level = 0.99, nsim_mean = 200)

  width_90 <- pred_mean_90$fit$upper - pred_mean_90$fit$lower
  width_99 <- pred_mean_99$fit$upper - pred_mean_99$fit$lower
  expect_true(width_99 > width_90)
})

test_that("Density prediction returns data frame with grid", {
  skip_if_not_test_level("ci")

  fit <- .get_fit_uncond()
  y_grid <- seq(0.1, 2, length.out = 10)
  pred_dens <- predict(fit, type = "density", y = y_grid, interval = "credible")

  expect_s3_class(pred_dens, "mixgpd_predict")
  expect_equal(pred_dens$type, "density")
  expect_s3_class(pred_dens$fit, "data.frame")
  expect_equal(nrow(pred_dens$fit), length(y_grid))
  expect_equal(pred_dens$grid, y_grid)

  density_col <- which(names(pred_dens$fit) %in% c("density", "estimate"))[1]
  expect_true(all(pred_dens$fit[[density_col]] >= 0))
  expect_silent(p <- plot(pred_dens))
})

test_that("Survival prediction returns data frame with probabilities", {
  skip_if_not_test_level("ci")

  fit <- .get_fit_uncond()
  y_grid <- seq(0.1, 2, length.out = 10)
  pred_surv <- predict(fit, type = "survival", y = y_grid, interval = "credible")

  expect_s3_class(pred_surv, "mixgpd_predict")
  expect_equal(pred_surv$type, "survival")
  expect_s3_class(pred_surv$fit, "data.frame")
  expect_true("survival" %in% names(pred_surv$fit))
  expect_equal(nrow(pred_surv$fit), length(y_grid))
  expect_true(all(pred_surv$fit$survival >= 0))
  expect_true(all(pred_surv$fit$survival <= 1))
  expect_true(cor(pred_surv$fit$y, pred_surv$fit$survival) < 0)
  expect_silent(p <- plot(pred_surv))
})

test_that("Sample prediction returns vector of samples", {
  skip_if_not_test_level("ci")

  fit <- .get_fit_uncond()
  nsim <- 100
  pred_samp <- predict(fit, type = "sample", nsim = nsim)

  expect_s3_class(pred_samp, "mixgpd_predict")
  expect_equal(pred_samp$type, "sample")
  expect_type(pred_samp$fit, "double")
  expect_equal(length(pred_samp$fit), nsim)
  expect_true(all(pred_samp$fit > 0))
  expect_silent(p <- plot(pred_samp))
})

test_that("All prediction types have consistent class structure", {
  skip_if_not_test_level("ci")

  fit <- .get_fit_uncond()
  pred_types <- list(
    quantile = predict(fit, type = "quantile"),
    mean = predict(fit, type = "mean", nsim_mean = 100),
    density = predict(fit, type = "density", y = seq(0.1, 2, length.out = 5)),
    survival = predict(fit, type = "survival", y = seq(0.1, 2, length.out = 5)),
    sample = predict(fit, type = "sample", nsim = 50)
  )

  for (pred_name in names(pred_types)) {
    expect_s3_class(pred_types[[pred_name]], "mixgpd_predict")
    expect_equal(pred_types[[pred_name]]$type, pred_name)
    expect_silent(plot(pred_types[[pred_name]]))
  }
})

# =============================================================================
# Quantile branches (from test-quantile-branches.R)
# =============================================================================
test_that("Quantile helpers handle log.p and lower.tail branches (mix + mixgpd)", {
  w3 <- c(0.2, 0.3, 0.5)
  p <- c(0.1, 0.5, 0.9)

  check_q_branches <- function(qfun, args, tol = 1e-5) {
    q_plain <- do.call(qfun, c(list(p = p), args))
    q_logp <- do.call(qfun, c(list(p = log(p), log.p = TRUE), args))
    expect_equal(q_logp, q_plain, tolerance = tol)

    q_upper <- do.call(qfun, c(list(p = p, lower.tail = FALSE), args))
    q_ref <- do.call(qfun, c(list(p = 1 - p), args))
    expect_equal(q_upper, q_ref, tolerance = tol)

    q_edge <- do.call(qfun, c(list(p = c(0, 1)), args))
    expect_true(is.numeric(q_edge))
    expect_equal(length(q_edge), 2L)
  }

  check_q_branches(
    qCauchyMix,
    list(w = w3, location = c(-1, 1, 3), scale = c(1.2, 0.8, 0.6)),
    tol = 1e-4
  )
  check_q_branches(
    qNormMix,
    list(w = w3, mean = c(-1, 0.5, 2), sd = c(1.0, 0.6, 1.8)),
    tol = 1e-4
  )
  check_q_branches(
    qLaplaceMix,
    list(w = w3, location = c(-1, 0, 1.5), scale = c(0.6, 1.0, 1.4)),
    tol = 1e-4
  )
  check_q_branches(
    qGammaMix,
    list(w = w3, shape = c(2, 3, 5), scale = c(1.0, 0.7, 1.2)),
    tol = 1e-4
  )
  check_q_branches(
    qLognormalMix,
    list(w = w3, meanlog = c(0, 0.3, 0.6), sdlog = c(0.5, 0.7, 0.9)),
    tol = 1e-4
  )
  check_q_branches(
    qInvGaussMix,
    list(w = w3, mean = c(1.5, 2.5, 3.5), shape = c(4, 6, 8)),
    tol = 1e-4
  )
  check_q_branches(
    qAmorosoMix,
    list(
      w = w3,
      loc = c(0, 0.3, 0.6),
      scale = c(1, 1, 1),
      shape1 = c(2, 3, 5),
      shape2 = c(1.0, 1.3, 1.6)
    ),
    tol = 2e-4
  )

  gpd_tail <- list(threshold = 3, tail_scale = 1.0, tail_shape = 0.2)
  check_q_branches(
    qInvGaussMixGpd,
    c(list(w = w3, mean = c(1.5, 2.5, 3.5), shape = c(4, 6, 8)), gpd_tail),
    tol = 2e-4
  )
  check_q_branches(
    qAmorosoMixGpd,
    c(
      list(
        w = w3,
        loc = c(0, 0.3, 0.6),
        scale = c(1, 1, 1),
        shape1 = c(2, 3, 5),
        shape2 = c(1.0, 1.3, 1.6)
      ),
      gpd_tail
    ),
    tol = 2e-4
  )
})
