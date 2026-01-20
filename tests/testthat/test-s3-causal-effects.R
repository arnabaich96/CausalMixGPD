# test-s3-causal-effects.R
# S3 print/summary methods for QTE/ATE objects (Tier B - requires MCMC)

# Helper to get/cache a causal fit
get_causal_fit <- function() {
  cache_key <- "test_causal_fit_s3"
  cached <- get0(cache_key, envir = .GlobalEnv, ifnotfound = NULL)
  if (!is.null(cached)) return(cached)
  
  set.seed(123)
  n <- 30
  X <- cbind(x1 = stats::rnorm(n), x2 = stats::runif(n, -1, 1))
  T <- stats::rbinom(n, 1, stats::plogis(0.2 + 0.4 * X[, 1]))
  y <- abs(stats::rnorm(n)) + 0.1
  
  mcmc_out <- list(niter = 30, nburnin = 10, thin = 1, nchains = 1, seed = 1)
  mcmc_ps  <- list(niter = 30, nburnin = 10, thin = 1, nchains = 1, seed = 1)
  
  cb <- DPmixGPD::build_causal_bundle(
    y = y,
    X = X,
    T = T,
    backend = c("sb", "sb"),
    kernel = c("normal", "normal"),
    GPD = c(FALSE, FALSE),
    components = c(4, 4),
    mcmc_outcome = mcmc_out,
    mcmc_ps = mcmc_ps
  )
  
  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  assign(cache_key, cf, envir = .GlobalEnv)
  cf
}

# ============================================================
# QTE Tests
# ============================================================

test_that("qte() returns proper dpmixgpd_qte class with expected fields", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  
  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5, 0.75), interval = "credible")
  
  # Class check

  expect_s3_class(q, "dpmixgpd_qte")
  
  # Required fields exist
  expect_true(all(c("fit", "probs", "grid", "trt", "con", "type") %in% names(q)))
  expect_equal(q$type, "qte")
  expect_equal(q$probs, c(0.25, 0.5, 0.75))
  
  # QTE fit data frame structure
  expect_true("qte" %in% names(q))
  expect_true(is.data.frame(q$qte$fit))
  expect_true(all(c("estimate", "lower", "upper") %in% names(q$qte$fit)))
  
  # Metadata present
  expect_true("meta" %in% names(q))
  expect_true("level" %in% names(q))
  expect_true("interval" %in% names(q))
})

test_that("print.dpmixgpd_qte produces readable output", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), interval = "credible")
  
  # Capture print output
  out <- capture.output(print(q))
  
  # Check key elements in output
  expect_true(any(grepl("QTE", out, fixed = TRUE)))
  expect_true(any(grepl("Prediction points", out, fixed = TRUE)))
  expect_true(any(grepl("Quantile grid", out, fixed = TRUE)))
  expect_true(any(grepl("Credible interval", out, fixed = TRUE)))
  expect_true(any(grepl("estimate", out, fixed = TRUE)))
  
  # Returns invisibly
  invisible_result <- print(q)
  expect_identical(invisible_result, q)
})

test_that("summary.dpmixgpd_qte returns structured summary object", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5, 0.75), interval = "credible")
  
  s <- summary(q)
  
  # Class check
  expect_s3_class(s, "summary.dpmixgpd_qte")
  
  # Required summary components
  expect_true(all(c("overall", "quantile_summary", "ci_summary", "meta") %in% names(s)))
  
  # Overall summary fields
  expect_true(all(c("n_pred", "n_quantiles", "quantiles", "level", "interval") %in% names(s$overall)))
  expect_equal(s$overall$n_quantiles, 3L)
  expect_equal(s$overall$quantiles, c(0.25, 0.5, 0.75))
  
  # Quantile summary table
  expect_true(is.data.frame(s$quantile_summary))
  expect_true(all(c("quantile", "mean_qte", "median_qte") %in% names(s$quantile_summary)))
  expect_equal(nrow(s$quantile_summary), 3L)
  
  # CI summary present (since interval = "credible")
  expect_true(!is.null(s$ci_summary))
  expect_true(all(c("mean_width", "median_width") %in% names(s$ci_summary)))
})

test_that("print.summary.dpmixgpd_qte produces formatted output", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), interval = "credible")
  s <- summary(q)
  
  out <- capture.output(print(s))
  
  # Check formatted sections
  expect_true(any(grepl("QTE Summary", out, fixed = TRUE)))
  expect_true(any(grepl("Prediction points", out, fixed = TRUE)))
  expect_true(any(grepl("QTE by quantile", out, fixed = TRUE)))
  expect_true(any(grepl("Credible interval width", out, fixed = TRUE)))
  
  # Returns invisibly
  invisible_result <- print(s)
  expect_identical(invisible_result, s)
})

# ============================================================
# ATE Tests
# ============================================================

test_that("ate() returns proper dpmixgpd_ate class with expected fields", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  
  a <- DPmixGPD::ate(cf, interval = "credible", nsim_mean = 30L)
  
  # Class check
  expect_s3_class(a, "dpmixgpd_ate")
  
  # Required fields exist
  expect_true(all(c("fit", "trt", "con", "type") %in% names(a)))
  expect_equal(a$type, "ate")
  
  # ATE fit data frame structure
  expect_true("ate" %in% names(a))
  expect_true(is.data.frame(a$ate$fit))
  expect_true(all(c("estimate", "lower", "upper") %in% names(a$ate$fit)))
  
  # Metadata present
  expect_true("meta" %in% names(a))
  expect_true("level" %in% names(a))
  expect_true("interval" %in% names(a))
  expect_true("nsim_mean" %in% names(a))
})

test_that("print.dpmixgpd_ate produces readable output", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "credible")
  
  # Capture print output
  out <- capture.output(print(a))
  
  # Check key elements in output
  expect_true(any(grepl("ATE", out, fixed = TRUE)))
  expect_true(any(grepl("Prediction points", out, fixed = TRUE)))
  expect_true(any(grepl("Credible interval", out, fixed = TRUE)))
  expect_true(any(grepl("estimate", out, fixed = TRUE)))
  
  # Returns invisibly
  invisible_result <- print(a)
  expect_identical(invisible_result, a)
})

test_that("summary.dpmixgpd_ate returns structured summary object", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "credible", nsim_mean = 30L)
  
  s <- summary(a)
  
  # Class check
  expect_s3_class(s, "summary.dpmixgpd_ate")
  
  # Required summary components
  expect_true(all(c("overall", "ate_stats", "ci_summary", "meta") %in% names(s)))
  
  # Overall summary fields
  expect_true(all(c("n_pred", "level", "interval", "nsim_mean") %in% names(s$overall)))
  
  # ATE stats present
  expect_true(!is.null(s$ate_stats))
  expect_true(all(c("mean_ate", "median_ate", "min_ate", "max_ate") %in% names(s$ate_stats)))
  
  # CI summary present (since interval = "credible")
  expect_true(!is.null(s$ci_summary))
  expect_true(all(c("mean_width", "median_width") %in% names(s$ci_summary)))
})

test_that("print.summary.dpmixgpd_ate produces formatted output", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "credible")
  s <- summary(a)
  
  out <- capture.output(print(s))
  
  # Check formatted sections
  expect_true(any(grepl("ATE Summary", out, fixed = TRUE)))
  expect_true(any(grepl("Prediction points", out, fixed = TRUE)))
  expect_true(any(grepl("ATE statistics", out, fixed = TRUE)))
  expect_true(any(grepl("Credible interval width", out, fixed = TRUE)))
  
  # Returns invisibly
  invisible_result <- print(s)
  expect_identical(invisible_result, s)
})

# ============================================================
# Plot Tests
# ============================================================

test_that("plot.dpmixgpd_qte builds without error", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")
  
  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), interval = "credible")
  
  # Should return plot list without error
  p <- plot(q)
  expect_true(is.list(p))
  expect_s3_class(p, "dpmixgpd_causal_predict_plots")
  expect_true(all(c("trt_control", "treatment_effect") %in% names(p)))
  
  # Each plot should be ggplot
  expect_s3_class(p$trt_control, "ggplot")
  expect_s3_class(p$treatment_effect, "ggplot")
  
  # Plots should build without error
  expect_error(ggplot2::ggplot_build(p$trt_control), NA)
  expect_error(ggplot2::ggplot_build(p$treatment_effect), NA)
})

test_that("plot.dpmixgpd_ate builds without error", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")
  
  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "credible")
  
  # Should return plot list without error
  p <- plot(a)
  expect_true(is.list(p))
  expect_s3_class(p, "dpmixgpd_causal_predict_plots")
  expect_true(all(c("trt_control", "treatment_effect") %in% names(p)))
  
  # Each plot should be ggplot
  expect_s3_class(p$trt_control, "ggplot")
  expect_s3_class(p$treatment_effect, "ggplot")
  
  # Plots should build without error
  expect_error(ggplot2::ggplot_build(p$trt_control), NA)
  expect_error(ggplot2::ggplot_build(p$treatment_effect), NA)
})

# ============================================================
# Edge Cases
# ============================================================

test_that("QTE with interval='none' omits CI fields gracefully", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  q <- DPmixGPD::qte(cf, probs = c(0.5), interval = "none")
  
  expect_s3_class(q, "dpmixgpd_qte")
  expect_true(is.null(q$lower) || all(is.na(q$lower)))
  expect_true(is.null(q$upper) || all(is.na(q$upper)))
  
  # Print should still work
  out <- capture.output(print(q))
  expect_true(any(grepl("QTE", out, fixed = TRUE)))
  
  # Summary should handle missing CI
  s <- summary(q)
  expect_true(is.null(s$ci_summary) || length(s$ci_summary) == 0)
})

test_that("ATE with interval='none' omits CI fields gracefully", {
  skip_if_not_test_level("ci")
  
  cf <- get_causal_fit()
  a <- DPmixGPD::ate(cf, interval = "none")
  
  expect_s3_class(a, "dpmixgpd_ate")
  expect_true(is.null(a$lower) || all(is.na(a$lower)))
  expect_true(is.null(a$upper) || all(is.na(a$upper)))
  
  # Print should still work
  out <- capture.output(print(a))
  expect_true(any(grepl("ATE", out, fixed = TRUE)))
  
  # Summary should handle missing CI
  s <- summary(a)
  expect_true(is.null(s$ci_summary) || length(s$ci_summary) == 0)
})
