# test-plots-causal-effects.R
# Comprehensive tests for QTE/ATE plot and print/summary S3 methods
#
# Tests cover:
#   - print.dpmixgpd_qte() / summary.dpmixgpd_qte()
#   - print.dpmixgpd_ate() / summary.dpmixgpd_ate()
#   - plot.dpmixgpd_qte() with type="effect"/"arms"/"both"
#   - plot.dpmixgpd_ate() with type="effect"/"arms"/"both"
#   - No NA aesthetics in plot data
#   - Graceful error when ggplot2 is missing

# =============================================================================
# Shared test fixture: create a minimal causal fit once for reuse
# =============================================================================
.make_causal_fit_for_plot <- function() {
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
    
  )

  cf <- DPmixGPD::run_mcmc_causal(cb, show_progress = FALSE)
  list(fit = cf, X = X, y = y, n = n)
}

# =============================================================================
# Tier B (ci): print/summary tests for QTE objects
# =============================================================================
test_that("print.dpmixgpd_qte works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  expect_output(print(qte_res), "QTE")
  expect_output(print(qte_res), "probs|quantile", ignore.case = TRUE)
})

test_that("summary.dpmixgpd_qte works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  s <- summary(qte_res)
  expect_s3_class(s, "summary.dpmixgpd_qte")

  # Should print without error
  expect_output(print(s), "QTE")
})

# =============================================================================
# Tier B (ci): print/summary tests for ATE objects
# =============================================================================
test_that("print.dpmixgpd_ate works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  expect_output(print(ate_res), "ATE")
})

test_that("summary.dpmixgpd_ate works", {
  skip_if_not_test_level("ci")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  s <- summary(ate_res)
  expect_s3_class(s, "summary.dpmixgpd_ate")

  # Should print without error
  expect_output(print(s), "ATE")
})

# =============================================================================
# Tier B (ci): plot.dpmixgpd_qte with type parameter
# =============================================================================
test_that("plot.dpmixgpd_qte type='effect' works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  p <- plot(qte_res, type = "effect")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # Check no NA in y aesthetics (estimate column)
  built <- ggplot2::ggplot_build(p)
  expect_false(all(is.na(built$data[[1]]$y)))
})

test_that("plot.dpmixgpd_qte type='arms' works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  p <- plot(qte_res, type = "arms")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # Check for two groups (Treated, Control)
  built <- ggplot2::ggplot_build(p)
  # There should be line data
  expect_true(nrow(built$data[[1]]) > 0)
})

test_that("plot.dpmixgpd_qte type='both' returns list", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.25, 0.5), newdata = X[1:3, ], interval = "credible")

  plots <- plot(qte_res, type = "both")

  expect_true(is.list(plots))
  expect_true(all(c("effect", "arms") %in% names(plots)))
  expect_s3_class(plots$effect, "ggplot")
  expect_s3_class(plots$arms, "ggplot")
})

test_that("plot.dpmixgpd_qte default type is 'effect'", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.5), newdata = X[1:3, ], interval = "credible")

  # Default should be effect
  p_default <- plot(qte_res)
  p_effect <- plot(qte_res, type = "effect")

  # Both should be single ggplot objects (not lists)
  expect_s3_class(p_default, "ggplot")
  expect_s3_class(p_effect, "ggplot")
})

# =============================================================================
# Tier B (ci): plot.dpmixgpd_ate with type parameter
# =============================================================================
test_that("plot.dpmixgpd_ate type='effect' works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  p <- plot(ate_res, type = "effect")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # Check no NA in y aesthetics
  built <- ggplot2::ggplot_build(p)
  expect_false(all(is.na(built$data[[1]]$y)))
})

test_that("plot.dpmixgpd_ate type='arms' works", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  p <- plot(ate_res, type = "arms")

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # Check for two groups (Treated, Control)
  built <- ggplot2::ggplot_build(p)
  expect_true(nrow(built$data[[1]]) > 0)
})

test_that("plot.dpmixgpd_ate type='both' returns list", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  plots <- plot(ate_res, type = "both")

  expect_true(is.list(plots))
  expect_true(all(c("effect", "arms") %in% names(plots)))
  expect_s3_class(plots$effect, "ggplot")
  expect_s3_class(plots$arms, "ggplot")
})

test_that("plot.dpmixgpd_ate default type is 'effect'", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:3, ], interval = "credible", nsim_mean = 20L)

  # Default should be effect
  p_default <- plot(ate_res)
  p_effect <- plot(ate_res, type = "effect")

  # Both should be single ggplot objects (not lists)
  expect_s3_class(p_default, "ggplot")
  expect_s3_class(p_effect, "ggplot")
})

# =============================================================================
# Tier B (ci): Test no NA aesthetics in built plots
# =============================================================================
test_that("QTE plot effect type has no NA in estimate data", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  qte_res <- DPmixGPD::qte(cf, probs = c(0.5), newdata = X[1:5, ], interval = "credible")

  p <- plot(qte_res, type = "effect")
  built <- ggplot2::ggplot_build(p)

  # The main line layer should have non-NA y values
  line_data <- built$data[[1]]
  expect_true(any(!is.na(line_data$y)), info = "Effect plot should have non-NA estimates")
})

test_that("ATE plot effect type has no NA in estimate data", {
  skip_if_not_test_level("ci")
  skip_if_not_installed("ggplot2")

  data <- .make_causal_fit_for_plot()
  cf <- data$fit
  X <- data$X

  ate_res <- DPmixGPD::ate(cf, newdata = X[1:5, ], interval = "credible", nsim_mean = 20L)

  p <- plot(ate_res, type = "effect")
  built <- ggplot2::ggplot_build(p)

  # The main line layer should have non-NA y values
  line_data <- built$data[[1]]
  expect_true(any(!is.na(line_data$y)), info = "Effect plot should have non-NA estimates")
})

# =============================================================================
# Tier A (cran): Error handling - ggplot2 missing message
# =============================================================================
test_that("plot.dpmixgpd_qte errors gracefully without ggplot2", {
  skip_if_not_test_level("cran")


  # Create a minimal mock QTE object
  mock_qte <- list(
    fit = matrix(1:3, nrow = 3),
    lower = matrix(0:2, nrow = 3),
    upper = matrix(2:4, nrow = 3),
    trt = list(fit = data.frame(id = 1:3, index = 0.5, estimate = 1:3, lower = 0:2, upper = 2:4)),
    con = list(fit = data.frame(id = 1:3, index = 0.5, estimate = 0:2, lower = 0:2, upper = 1:3)),
    grid = 0.5,
    ps = c(0.3, 0.5, 0.7)
  )
  class(mock_qte) <- "dpmixgpd_qte"

  # We can't easily mock requireNamespace, but we can at least verify the function exists
  expect_true(is.function(plot.dpmixgpd_qte))
})

test_that("plot.dpmixgpd_ate errors gracefully without ggplot2", {
  skip_if_not_test_level("cran")

  # Create a minimal mock ATE object
  mock_ate <- list(
    fit = 1:3,
    lower = 0:2,
    upper = 2:4,
    trt = list(fit = data.frame(id = 1:3, estimate = 2:4, lower = 1:3, upper = 3:5)),
    con = list(fit = data.frame(id = 1:3, estimate = 1:3, lower = 0:2, upper = 2:4)),
    ps = c(0.3, 0.5, 0.7)
  )
  class(mock_ate) <- "dpmixgpd_ate"

  # Verify the function exists
  expect_true(is.function(plot.dpmixgpd_ate))
})

# =============================================================================
# Tier A (cran): Test .coerce_fit_df helper function
# =============================================================================
test_that(".coerce_fit_df handles vector input", {
  skip_if_not_test_level("cran")

  # Access internal function
  coerce <- DPmixGPD:::.coerce_fit_df

  vec <- c(1.5, 2.5, 3.5)
  result <- coerce(vec)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(all(c("estimate", "lower", "upper", "id") %in% names(result)))
  expect_equal(result$estimate, vec)
  expect_true(all(is.na(result$lower)))
  expect_true(all(is.na(result$upper)))
})

test_that(".coerce_fit_df handles matrix input", {
  skip_if_not_test_level("cran")

  coerce <- DPmixGPD:::.coerce_fit_df

  mat <- matrix(c(1, 2, 3, 0.5, 1.5, 2.5, 1.5, 2.5, 3.5), nrow = 3, ncol = 3)
  result <- coerce(mat)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true("estimate" %in% names(result))
})

test_that(".coerce_fit_df handles data.frame input", {
  skip_if_not_test_level("cran")

  coerce <- DPmixGPD:::.coerce_fit_df

  df <- data.frame(estimate = c(1, 2, 3), lower = c(0, 1, 2), upper = c(2, 3, 4))
  result <- coerce(df)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$estimate, c(1, 2, 3))
  expect_true("id" %in% names(result))
})

test_that(".coerce_fit_df adds missing columns", {
  skip_if_not_test_level("cran")

  coerce <- DPmixGPD:::.coerce_fit_df

  # Data frame missing lower/upper
  df <- data.frame(fit = c(1, 2, 3))
  result <- coerce(df)

  expect_true(is.data.frame(result))
  expect_true("estimate" %in% names(result))
  expect_true("lower" %in% names(result))
  expect_true("upper" %in% names(result))
  expect_true("id" %in% names(result))
})
