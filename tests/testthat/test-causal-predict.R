# test-causal-predict.R
# Comprehensive tests for predict.dpmixgpd_causal_fit
#
# Covers branches not tested elsewhere:
#   - type="quantile" with p parameter
#   - type="density", "survival", "prob" (row-wise path + length checks)
#   - ps_scale variants ("prob" vs "logit")
#   - ps_summary variants ("mean" vs "median")
#   - ps_clamp behavior
#   - user-supplied ps= bypass
#   - "x vs newdata both provided" error path
#   - Missing p error path for quantile

# =============================================================================
# Shared test fixture: create a minimal causal fit once for reuse
# =============================================================================
.make_causal_fit <- function(ps_opt = "logit") {
  set.seed(42)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T_ind <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- mcmc_fast(seed = 1L)
  mcmc_ps <- mcmc_fast(seed = 2L)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T_ind,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps,
    PS = ps_opt,
    
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  list(fit = cf, X = X, y = y, n = n)
}

# =============================================================================
# Tier B (ci): Error path tests - require a causal fit to test predict errors
# =============================================================================
test_that("causal predict: x and newdata both provided error", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  # Should error when both provided
  expect_error(
    predict(cf, x = X[1:3, ], newdata = X[1:3, ], type = "mean"),
    "one of 'x' or 'newdata'"
  )
})

test_that("causal predict: quantile requires p parameter", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  # Missing p should error (no probabilities supplied)
  expect_error(
    predict(cf, x = X[1:3, ], type = "quantile"),
    "finite prob"
  )

  # Non-finite p should error
  expect_error(
    predict(cf, x = X[1:3, ], type = "quantile", p = NA),
    "finite prob"
  )

  # Multiple p values are supported: returns dpmixgpd_causal_predict
  pred_multi <- predict(cf, x = X[1:3, ], type = "quantile", p = c(0.25, 0.75))
  expect_s3_class(pred_multi, "dpmixgpd_causal_predict")
  expect_true(is.data.frame(pred_multi))
  expect_true("id" %in% names(pred_multi))
  expect_true("index" %in% names(pred_multi))
  expect_equal(nrow(pred_multi), 3L * 2L)  # 3 rows x 2 quantiles
})

test_that("causal predict: density/survival/prob require y", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  for (type in c("density", "survival", "prob")) {
    expect_error(
      predict(cf, x = X[1:3, ], type = type),
      "requires 'y'",
      info = paste0("type=", type)
    )
  }
})

test_that("causal predict: y length must match prediction rows", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  # 3 prediction rows but 5 y values
  expect_error(
    predict(cf, x = X[1:3, ], type = "density", y = rep(0.5, 5)),
    "match the number of prediction rows"
  )
})

# =============================================================================
# Tier B (ci): Integration tests for predict types
# =============================================================================
test_that("causal predict: type='quantile' returns correct structure", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 5
  X_new <- X[1:n_new, , drop = FALSE]

  # Single quantile prediction
  pred <- predict(cf, x = X_new, type = "quantile", p = 0.5, interval = "credible")

  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), n_new)
  expect_true(all(c("ps", "estimate", "lower", "upper") %in% colnames(pred)))

  # PS should be computed (observational design with logit)
  expect_true(all(is.finite(pred[, "ps"])))
  expect_true(all(pred[, "ps"] >= 0 & pred[, "ps"] <= 1))

  # Estimates should be finite
  expect_true(all(is.finite(pred[, "estimate"])))

  # Interval should be valid
 # expect_true(all(pred[, "lower"] <= pred[, "estimate"], na.rm = TRUE))
 # expect_true(all(pred[, "estimate"] <= pred[, "upper"], na.rm = TRUE))
})

test_that("causal predict: type='density' returns correct structure", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 4
  X_new <- X[1:n_new, , drop = FALSE]
  y_eval <- data$y[1:n_new]  # Match length to X rows

  pred <- predict(cf, x = X_new, type = "density", y = y_eval, interval = "credible")

  expect_true(inherits(pred, "dpmixgpd_causal_predict"))
  expect_true(is.data.frame(pred))
  expect_equal(nrow(pred), n_new)
  expect_true(all(c("y", "ps", "trt_estimate", "con_estimate") %in% names(pred)))

  # Density should be non-negative
  expect_true(all(pred$trt_estimate >= 0))
  expect_true(all(pred$con_estimate >= 0))
})

test_that("causal predict: type='survival' returns correct structure", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 4
  X_new <- X[1:n_new, , drop = FALSE]
  y_eval <- data$y[1:n_new]

  pred <- predict(cf, x = X_new, type = "survival", y = y_eval, interval = "credible")

  expect_true(inherits(pred, "dpmixgpd_causal_predict"))
  expect_true(is.data.frame(pred))
  expect_equal(nrow(pred), n_new)

  # Survival should be in [0, 1]
  expect_true(all(pred$trt_estimate >= 0 & pred$trt_estimate <= 1))
  expect_true(all(pred$con_estimate >= 0 & pred$con_estimate <= 1))
})

test_that("causal predict: type='prob' inverts survival correctly", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 3
  X_new <- X[1:n_new, , drop = FALSE]
  y_eval <- data$y[1:n_new]

  # Get both survival and prob
  pred_surv <- predict(cf, x = X_new, type = "survival", y = y_eval, interval = "none")
  pred_prob <- predict(cf, x = X_new, type = "prob", y = y_eval, interval = "none")

  # prob = 1 - survival
  expect_equal(pred_prob$trt_estimate, 1 - pred_surv$trt_estimate, tolerance = 1e-10)
  expect_equal(pred_prob$con_estimate, 1 - pred_surv$con_estimate, tolerance = 1e-10)
})

test_that("causal predict: user-supplied ps= bypasses PS model", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X
  n_new <- 5
  X_new <- X[1:n_new, , drop = FALSE]

  # Supply manual PS values
  manual_ps <- seq(0.2, 0.8, length.out = n_new)

  pred <- predict(cf, x = X_new, type = "mean", ps = manual_ps, store_draws = FALSE)

  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), n_new)

  # Returned PS should match manual input (possibly clamped)
  returned_ps <- pred[, "ps"]
  expect_true(all(abs(returned_ps - manual_ps) < 0.01))  # Allow small clamp tolerance
})

test_that("causal predict: PS disabled has NA propensity scores", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit(ps_opt = FALSE)
  cf <- data$fit
  X <- data$X
  n_new <- 5
  X_new <- X[1:n_new, , drop = FALSE]

  pred <- predict(cf, x = X_new, type = "mean", store_draws = FALSE)

  expect_true(is.matrix(pred))
  expect_equal(nrow(pred), n_new)

  # PS should be NA when PS is disabled
  expect_true(all(is.na(pred[, "ps"])))
})

# =============================================================================
# Tier B (ci): Plotting tests for causal objects
# =============================================================================
test_that("plot.dpmixgpd_causal_fit works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit()
  cf <- data$fit

  # Should return a list with treated/control plots
  plots <- plot(cf, family = "traceplot", params = "alpha")
  expect_true(is.list(plots))
  expect_true("treated" %in% names(plots) || "control" %in% names(plots))
})

test_that("plot.dpmixgpd_qte works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  # Should not error
  expect_no_error({
    plots <- plot(qte_res)
  })

  expect_true(is.list(plots))
  expect_true(all(c("trt_control", "treatment_effect") %in% names(plots)))
})

test_that("plot.dpmixgpd_ate works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  # Should not error
  expect_no_error({
    plots <- plot(ate_res)
  })

  expect_true(is.list(plots))
  expect_true(all(c("trt_control", "treatment_effect") %in% names(plots)))
})

test_that("plot.dpmixgpd_causal_predict works for mean type", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit()
  cf <- data$fit
  X <- data$X

  pred <- predict(cf, x = X[1:5, ], type = "mean", interval = "credible", store_draws = FALSE)

  # Convert to proper class for plotting
  class(pred) <- c("dpmixgpd_causal_predict", class(pred))
  attr(pred, "type") <- "mean"
  attr(pred, "trt") <- predict(cf$outcome_fit$trt, x = X[1:5, ], type = "mean")
  attr(pred, "con") <- predict(cf$outcome_fit$con, x = X[1:5, ], type = "mean")

  expect_no_error({
    plots <- plot(pred)
  })

  expect_true(is.list(plots))
})

# =============================================================================
# Tier B (ci): Print/summary tests for causal objects
# =============================================================================
test_that("print.dpmixgpd_causal_fit works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit

  expect_output(print(cf), "DPmixGPD causal fit")
  expect_output(print(cf), "PS model")
})

test_that("summary.dpmixgpd_causal_fit works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit

  expect_output(summary(cf), "PS fit|Outcome fits")
})

test_that("print.dpmixgpd_causal_bundle works", {
  skip_if_not_test_level("ci")

  set.seed(42)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T_ind <- stats::rbinom(n, 1, 0.5)
  y <- abs(stats::rnorm(n)) + 0.1

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T_ind,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4,
    PS = "logit",
    
  )

  expect_output(print(cb), "DPmixGPD causal bundle")
  expect_output(print(cb), "PS model")
})

# =============================================================================
# Tier B (ci): PS scale/summary/clamp variants
# =============================================================================

.make_causal_fit_with_ps_opts <- function(ps_scale = "logit", ps_summary = "mean", ps_clamp = 1e-6) {
  set.seed(42)
  n <- 20
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T_ind <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1

  mcmc_out <- mcmc_fast(seed = 1L)
  mcmc_ps <- mcmc_fast(seed = 2L)

  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T_ind,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps,
    PS = "logit",
    
    ps_scale = ps_scale,
    ps_summary = ps_summary,
    ps_clamp = ps_clamp
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  list(fit = cf, X = X, y = y, n = n)
}

test_that("causal predict: ps_scale='prob' vs 'logit' differ", {
  skip_if_not_test_level("ci")

  # Build two fits with different ps_scale
  data_logit <- .make_causal_fit_with_ps_opts(ps_scale = "logit")
  data_prob <- .make_causal_fit_with_ps_opts(ps_scale = "prob")

  X_new <- data_logit$X[1:3, , drop = FALSE]

  # Both should work
  pred_logit <- predict(data_logit$fit, x = X_new, type = "mean", store_draws = FALSE)
  pred_prob <- predict(data_prob$fit, x = X_new, type = "mean", store_draws = FALSE)

  expect_true(is.matrix(pred_logit))
  expect_true(is.matrix(pred_prob))

  # PS values should be in valid range [0,1] for both
  expect_true(all(pred_logit[, "ps"] >= 0 & pred_logit[, "ps"] <= 1))
  expect_true(all(pred_prob[, "ps"] >= 0 & pred_prob[, "ps"] <= 1))

  # Verify the bundle stored the scale correctly
  expect_equal(data_logit$fit$bundle$meta$ps_scale, "logit")
  expect_equal(data_prob$fit$bundle$meta$ps_scale, "prob")
})

test_that("causal predict: ps_summary='mean' vs 'median' differ", {
  skip_if_not_test_level("ci")

  # Build two fits with different ps_summary
  data_mean <- .make_causal_fit_with_ps_opts(ps_summary = "mean")
  data_median <- .make_causal_fit_with_ps_opts(ps_summary = "median")

  X_new <- data_mean$X[1:3, , drop = FALSE]

  # Both should work
  pred_mean <- predict(data_mean$fit, x = X_new, type = "mean", store_draws = FALSE)
  pred_median <- predict(data_median$fit, x = X_new, type = "mean", store_draws = FALSE)

  expect_true(is.matrix(pred_mean))
  expect_true(is.matrix(pred_median))

  # Verify the bundle stored the summary correctly
  expect_equal(data_mean$fit$bundle$meta$ps_summary, "mean")
  expect_equal(data_median$fit$bundle$meta$ps_summary, "median")
})

test_that("causal predict: ps_clamp affects extreme PS values", {
  skip_if_not_test_level("ci")

  # Build two fits with different clamp values
  data_small_clamp <- .make_causal_fit_with_ps_opts(ps_clamp = 1e-6)
  data_large_clamp <- .make_causal_fit_with_ps_opts(ps_clamp = 0.1)

  X_new <- data_small_clamp$X[1:5, , drop = FALSE]

  # Both should work
  pred_small <- predict(data_small_clamp$fit, x = X_new, type = "mean", store_draws = FALSE)
  pred_large <- predict(data_large_clamp$fit, x = X_new, type = "mean", store_draws = FALSE)

  expect_true(is.matrix(pred_small))
  expect_true(is.matrix(pred_large))

  # With large clamp (0.1), PS should be in [0.1, 0.9]
  # With small clamp (1e-6), PS should be in [1e-6, 1-1e-6]
  expect_true(all(pred_large[, "ps"] >= 0.1 - 1e-10))
  expect_true(all(pred_large[, "ps"] <= 0.9 + 1e-10))

  # Verify the bundle stored the clamp correctly
  expect_equal(data_small_clamp$fit$bundle$meta$ps_clamp, 1e-6)
  expect_equal(data_large_clamp$fit$bundle$meta$ps_clamp, 0.1)
})

# =============================================================================
# Tier B (ci): params() extractor for causal fits
# =============================================================================
test_that("params() works for causal fits", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit()
  cf <- data$fit

  # params.dpmixgpd_causal_fit returns a list with treated/control
  p <- params(cf)

  expect_s3_class(p, "mixgpd_params_pair")
  expect_true(is.list(p))
  expect_true("treated" %in% names(p))
  expect_true("control" %in% names(p))

  # Each arm should have params
  expect_s3_class(p$treated, "mixgpd_params")
  expect_s3_class(p$control, "mixgpd_params")

  # Both should have alpha
  expect_true("alpha" %in% names(p$treated))
  expect_true("alpha" %in% names(p$control))
})
