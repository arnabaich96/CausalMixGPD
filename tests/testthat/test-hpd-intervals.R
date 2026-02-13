# Test HPD interval functionality across all S3 functions
# Tests that interval = "hpd" works correctly and produces valid intervals

# ==============================================================================
# Test .compute_interval() helper function
# ==============================================================================

test_that(".compute_interval computes equal-tailed intervals correctly", {
  set.seed(42)
  draws <- rnorm(1000)

  iv <- DPmixGPD:::.compute_interval(draws, level = 0.95, type = "credible")

  expect_named(iv, c("lower", "upper"))
  expect_true(iv["lower"] < iv["upper"])
  expect_true(iv["lower"] < 0)  # For standard normal
  expect_true(iv["upper"] > 0)

  # Check approximate quantiles
  q <- quantile(draws, probs = c(0.025, 0.975))
  expect_equal(unname(iv["lower"]), unname(q[1]))
  expect_equal(unname(iv["upper"]), unname(q[2]))
})


test_that(".compute_interval computes HPD intervals correctly", {
  set.seed(42)
  draws <- rnorm(1000)

  iv <- DPmixGPD:::.compute_interval(draws, level = 0.95, type = "hpd")

  expect_named(iv, c("lower", "upper"))
  expect_true(iv["lower"] < iv["upper"])
  expect_true(iv["lower"] < 0)  # For standard normal
  expect_true(iv["upper"] > 0)

  # HPD should approximately match coda::HPDinterval
  hpd_coda <- coda::HPDinterval(coda::as.mcmc(draws), prob = 0.95)
  expect_equal(unname(iv["lower"]), unname(hpd_coda[1, "lower"]), tolerance = 0.001)
  expect_equal(unname(iv["upper"]), unname(hpd_coda[1, "upper"]), tolerance = 0.001)
})


test_that(".compute_interval handles skewed distributions differently", {
  set.seed(42)
  # Skewed distribution - HPD should give shorter interval
  draws <- rexp(1000)

  iv_credible <- DPmixGPD:::.compute_interval(draws, level = 0.95, type = "credible")
  iv_hpd <- DPmixGPD:::.compute_interval(draws, level = 0.95, type = "hpd")

  width_credible <- iv_credible["upper"] - iv_credible["lower"]
  width_hpd <- iv_hpd["upper"] - iv_hpd["lower"]

  # HPD interval should be narrower for skewed distributions
  expect_true(width_hpd <= width_credible + 0.01)
})


test_that(".compute_interval handles edge cases", {
  # Empty or NA draws
  iv <- DPmixGPD:::.compute_interval(c(NA, NA, NA), level = 0.95, type = "credible")
  expect_true(is.na(iv["lower"]))
  expect_true(is.na(iv["upper"]))

  # Single value
  iv <- DPmixGPD:::.compute_interval(c(1), level = 0.95, type = "credible")
  expect_true(is.na(iv["lower"]))
  expect_true(is.na(iv["upper"]))
})


# ==============================================================================
# Test predict.mixgpd_fit with interval parameter
# ==============================================================================

test_that("predict.mixgpd_fit supports unified interval parameter", {
  skip_if_not_test_level("ci")

  set.seed(42)
  y <- abs(rnorm(40)) + 0.2

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "gamma",
    GPD = FALSE,
    components = 6,
    mcmc = list(niter = 200, nburnin = 50, nchains = 1)
  )

  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  utils::capture.output(
    fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE),
    file = nullfile
  )

  # Test quantile prediction with both interval types
  pred_credible <- predict(fit, type = "quantile", index = c(0.5, 0.9),
                           interval = "credible")
  pred_hpd <- predict(fit, type = "quantile", index = c(0.5, 0.9),
                      interval = "hpd")
  pred_none <- predict(fit, type = "quantile", index = c(0.5, 0.9),
                       interval = NULL)

  expect_s3_class(pred_credible, "mixgpd_predict")
  expect_s3_class(pred_hpd, "mixgpd_predict")
  expect_s3_class(pred_none, "mixgpd_predict")

  # Both credible and hpd should have valid intervals
  expect_true(all(pred_credible$fit$lower <= pred_credible$fit$estimate))
  expect_true(all(pred_credible$fit$estimate <= pred_credible$fit$upper))
  expect_true(all(pred_hpd$fit$lower <= pred_hpd$fit$estimate))
  expect_true(all(pred_hpd$fit$estimate <= pred_hpd$fit$upper))

  # NULL interval should have NA for lower/upper
  expect_true(all(is.na(pred_none$fit$lower)))
  expect_true(all(is.na(pred_none$fit$upper)))

  # Default should be credible
  pred_default <- predict(fit, type = "quantile", index = c(0.5, 0.9))
  expect_equal(pred_default$fit$lower, pred_credible$fit$lower)
  expect_equal(pred_default$fit$upper, pred_credible$fit$upper)
})


test_that("predict.mixgpd_fit mean prediction works with HPD", {
  skip_if_not_test_level("ci")

  set.seed(42)
  y <- abs(rnorm(40)) + 0.2

  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "gamma",
    GPD = FALSE,
    components = 6,
    mcmc = list(niter = 200, nburnin = 50, nchains = 1)
  )

  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  utils::capture.output(
    fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE),
    file = nullfile
  )

  pred_credible <- predict(fit, type = "mean", interval = "credible", nsim_mean = 100)
  pred_hpd <- predict(fit, type = "mean", interval = "hpd", nsim_mean = 100)

  expect_s3_class(pred_credible, "mixgpd_predict")
  expect_s3_class(pred_hpd, "mixgpd_predict")

  # Both should have valid intervals
  expect_true(pred_credible$fit$lower[1] <= pred_credible$fit$estimate[1])
  expect_true(pred_hpd$fit$lower[1] <= pred_hpd$fit$estimate[1])
})


# ==============================================================================
# Test fitted.mixgpd_fit with interval parameter
# ==============================================================================

test_that("fitted.mixgpd_fit supports unified interval parameter", {
  skip_if_not_test_level("ci")

  set.seed(42)
  n <- 30
  y <- abs(rnorm(n)) + 0.2
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))

  bundle <- build_nimble_bundle(
    y = y,
    X = X,
    backend = "sb",
    kernel = "gamma",
    GPD = FALSE,
    components = 6,
    mcmc = list(niter = 200, nburnin = 50, nchains = 1)
  )

  nullfile <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  utils::capture.output(
    fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE),
    file = nullfile
  )

  fitted_credible <- fitted(fit, type = "mean", interval = "credible")
  fitted_hpd <- fitted(fit, type = "mean", interval = "hpd")
  fitted_none <- fitted(fit, type = "mean", interval = NULL)

  expect_s3_class(fitted_credible, "mixgpd_fitted")
  expect_s3_class(fitted_hpd, "mixgpd_fitted")
  expect_s3_class(fitted_none, "mixgpd_fitted")

  # Both should have valid structure
  expect_true(all(c("fit", "lower", "upper", "residuals") %in% names(fitted_credible)))
  expect_true(all(c("fit", "lower", "upper", "residuals") %in% names(fitted_hpd)))
  expect_true(all(c("fit", "lower", "upper", "residuals") %in% names(fitted_none)))

  # Check interval is stored
  expect_equal(attr(fitted_credible, "interval"), "credible")
  expect_equal(attr(fitted_hpd, "interval"), "hpd")
  expect_null(attr(fitted_none, "interval"))
})


# ==============================================================================
# Test .posterior_summarize with interval parameter
# ==============================================================================

test_that(".posterior_summarize uses interval correctly", {
  set.seed(42)
  draws <- matrix(rexp(500), nrow = 5, ncol = 100)

  summ_credible <- DPmixGPD:::.posterior_summarize(draws, interval = "credible")
  summ_hpd <- DPmixGPD:::.posterior_summarize(draws, interval = "hpd")
  summ_none <- DPmixGPD:::.posterior_summarize(draws, interval = NULL)

  # Check structure
  expect_named(summ_credible, c("estimate", "lower", "upper", "q"))
  expect_named(summ_hpd, c("estimate", "lower", "upper", "q"))
  expect_named(summ_none, c("estimate", "lower", "upper", "q"))

  # Estimates should be the same (all use mean)
  expect_equal(summ_credible$estimate, summ_hpd$estimate)
  expect_equal(summ_credible$estimate, summ_none$estimate)

  # Intervals should be different for skewed distribution (credible vs hpd)
  expect_false(all(summ_credible$lower == summ_hpd$lower))
  expect_false(all(summ_credible$upper == summ_hpd$upper))

  # NULL interval should give NA for lower/upper
  expect_true(all(is.na(summ_none$lower)))
  expect_true(all(is.na(summ_none$upper)))
})
