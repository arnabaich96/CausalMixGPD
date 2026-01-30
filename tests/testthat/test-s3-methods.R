# tests/testthat/test-s3-methods.R
# Unit tests for S3 print/summary/plot methods (04-S3-Methods.R)
# Uses minimal mock objects to avoid heavy MCMC runs

# Access S3 methods directly using ::: (needed since class dispatch may fail outside pkg)
print_mixgpd_params <- DPmixGPD:::print.mixgpd_params
print_mixgpd_params_pair <- DPmixGPD:::print.mixgpd_params_pair
print_dpmixgpd_qte <- DPmixGPD:::print.dpmixgpd_qte
print_dpmixgpd_ate <- DPmixGPD:::print.dpmixgpd_ate
summary_dpmixgpd_qte <- DPmixGPD:::summary.dpmixgpd_qte
summary_dpmixgpd_ate <- DPmixGPD:::summary.dpmixgpd_ate
print_summary_dpmixgpd_qte <- DPmixGPD:::print.summary.dpmixgpd_qte
print_summary_dpmixgpd_ate <- DPmixGPD:::print.summary.dpmixgpd_ate
plot_mixgpd_predict <- DPmixGPD:::plot.mixgpd_predict
print_dpmixgpd_causal_predict_plots <- DPmixGPD:::print.dpmixgpd_causal_predict_plots
print_mixgpd_predict_plots <- DPmixGPD:::print.mixgpd_predict_plots
print_mixgpd_fit_plots <- DPmixGPD:::print.mixgpd_fit_plots
print_dpmixgpd_causal_fit_plots <- DPmixGPD:::print.dpmixgpd_causal_fit_plots
plot_mixgpd_fitted <- DPmixGPD:::plot.mixgpd_fitted
print_mixgpd_fitted_plots <- DPmixGPD:::print.mixgpd_fitted_plots
plot_dpmixgpd_qte <- DPmixGPD:::plot.dpmixgpd_qte
plot_dpmixgpd_ate <- DPmixGPD:::plot.dpmixgpd_ate

# ======================================================================
# Mock object constructors
# ======================================================================

make_mock_mixgpd_params <- function(empty = FALSE) {
  if (empty) {
    out <- list()
  } else {
    out <- list(
      alpha = 1.5,
      w = c(0.6, 0.3, 0.1),
      mu = c(1, 2, 3),
      sigma = c(0.5, 1, 1.5),
      beta_mu = matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), nrow = 3, ncol = 2,
                       dimnames = list(paste0("comp", 1:3), c("x1", "x2")))
    )
  }
  class(out) <- "mixgpd_params"
  out
}

make_mock_mixgpd_params_pair <- function() {
  out <- list(
    treated = make_mock_mixgpd_params(),
    control = make_mock_mixgpd_params()
  )
  class(out) <- "mixgpd_params_pair"
  out
}

make_mock_qte <- function() {
  out <- list(
    probs = c(0.25, 0.5, 0.75),
    grid = c(0.25, 0.5, 0.75),
    n_pred = 5,
    level = 0.95,
    interval = "credible",
    meta = list(backend = list(trt = "sb", con = "sb"),
                kernel = list(trt = "normal", con = "normal"),
                GPD = list(trt = FALSE, con = FALSE)),
    ps = runif(5),
    x = matrix(rnorm(10), nrow = 5, ncol = 2),
    qte = list(
      fit = data.frame(
        id = rep(1:5, each = 3),
        index = rep(c(0.25, 0.5, 0.75), times = 5),
        estimate = rnorm(15),
        lower = rnorm(15) - 0.5,
        upper = rnorm(15) + 0.5
      )
    ),
    fit = matrix(rnorm(15), nrow = 5, ncol = 3),
    lower = matrix(rnorm(15) - 0.5, nrow = 5, ncol = 3),
    upper = matrix(rnorm(15) + 0.5, nrow = 5, ncol = 3),
    trt = list(fit = data.frame(estimate = rnorm(15), id = rep(1:5, 3), index = rep(c(0.25, 0.5, 0.75), each = 5))),
    con = list(fit = data.frame(estimate = rnorm(15), id = rep(1:5, 3), index = rep(c(0.25, 0.5, 0.75), each = 5)))
  )
  class(out) <- "dpmixgpd_qte"
  out
}

make_mock_ate <- function() {
  out <- list(
    n_pred = 5,
    level = 0.95,
    interval = "credible",
    nsim_mean = 200,
    meta = list(backend = list(trt = "sb", con = "sb"),
                kernel = list(trt = "normal", con = "normal"),
                GPD = list(trt = FALSE, con = FALSE)),
    ps = runif(5),
    x = matrix(rnorm(10), nrow = 5, ncol = 2),
    ate = list(
      fit = data.frame(
        id = 1:5,
        estimate = rnorm(5),
        lower = rnorm(5) - 0.5,
        upper = rnorm(5) + 0.5
      )
    ),
    fit = rnorm(5),
    lower = rnorm(5) - 0.5,
    upper = rnorm(5) + 0.5,
    trt = list(fit = data.frame(estimate = rnorm(5), id = 1:5)),
    con = list(fit = data.frame(estimate = rnorm(5), id = 1:5))
  )
  class(out) <- "dpmixgpd_ate"
  out
}

make_mock_mixgpd_predict <- function(type = "quantile") {
  if (type == "quantile") {
    out <- list(
      fit = data.frame(
        estimate = c(1, 2, 3),
        index = c(0.25, 0.5, 0.75),
        lower = c(0.5, 1.5, 2.5),
        upper = c(1.5, 2.5, 3.5)
      ),
      type = "quantile",
      grid = c(0.25, 0.5, 0.75)
    )
  } else if (type == "sample") {
    out <- list(
      fit = rnorm(100),
      type = "sample",
      grid = NULL
    )
  } else if (type == "mean") {
    out <- list(
      fit = data.frame(estimate = 5.2, lower = 4.8, upper = 5.6),
      type = "mean",
      grid = NULL
    )
  } else if (type == "density") {
    y_grid <- seq(0, 5, length.out = 20)
    out <- list(
      fit = data.frame(
        id = rep(1, 20),
        y = y_grid,
        density = dnorm(y_grid, mean = 2.5, sd = 1),
        lower = dnorm(y_grid, mean = 2.5, sd = 1) - 0.05,
        upper = dnorm(y_grid, mean = 2.5, sd = 1) + 0.05
      ),
      type = "density",
      grid = y_grid
    )
  } else if (type == "survival") {
    y_grid <- seq(0, 5, length.out = 20)
    out <- list(
      fit = data.frame(
        id = rep(1, 20),
        y = y_grid,
        survival = 1 - pnorm(y_grid, mean = 2.5, sd = 1),
        lower = pmax(0, 1 - pnorm(y_grid, mean = 2.5, sd = 1) - 0.05),
        upper = pmin(1, 1 - pnorm(y_grid, mean = 2.5, sd = 1) + 0.05)
      ),
      type = "survival",
      grid = y_grid
    )
  } else if (type == "location") {
    out <- list(
      fit = data.frame(
        mean = 5.0, mean_lower = 4.5, mean_upper = 5.5,
        median = 4.9, median_lower = 4.4, median_upper = 5.4
      ),
      type = "location",
      grid = NULL
    )
  } else {
    stop("Unknown mock predict type")
  }
  class(out) <- "mixgpd_predict"
  out
}

make_mock_causal_predict_plots <- function() {
  out <- list(
    trt_control = ggplot2::ggplot() + ggplot2::ggtitle("trt_control"),
    treatment_effect = ggplot2::ggplot() + ggplot2::ggtitle("treatment_effect")
  )
  class(out) <- c("dpmixgpd_causal_predict_plots", "list")
  out
}

make_mock_mixgpd_predict_plots <- function() {
  p <- ggplot2::ggplot() + ggplot2::ggtitle("predict_plot")
  class(p) <- c("mixgpd_predict_plots", class(p))
  p
}

make_mock_mixgpd_fit_plots <- function() {
  out <- list(
    traceplot = ggplot2::ggplot() + ggplot2::ggtitle("traceplot"),
    density = ggplot2::ggplot() + ggplot2::ggtitle("density")
  )
  class(out) <- c("mixgpd_fit_plots", "list")
  out
}

make_mock_mixgpd_fitted <- function() {
  n <- 20
  fit_vals <- rnorm(n, mean = 5)
  y_obs <- fit_vals + rnorm(n, sd = 0.5)
  out <- data.frame(
    fit = fit_vals,
    lower = fit_vals - 0.5,
    upper = fit_vals + 0.5,
    residuals = y_obs - fit_vals
  )
  class(out) <- c("mixgpd_fitted", "data.frame")

  # Attach a minimal mock object

mock_obj <- list(
    data = list(y = y_obs),
    spec = list(meta = list(backend = "sb", kernel = "normal", GPD = FALSE))
  )
  attr(out, "object") <- mock_obj
  attr(out, "level") <- 0.95
  attr(out, "interval") <- "credible"
  out
}

make_mock_causal_fit_plots <- function() {
  treated_plots <- list(
    traceplot = ggplot2::ggplot() + ggplot2::ggtitle("treated_trace"),
    density = ggplot2::ggplot() + ggplot2::ggtitle("treated_density")
  )
  class(treated_plots) <- c("mixgpd_fit_plots", "list")

  control_plots <- list(
    traceplot = ggplot2::ggplot() + ggplot2::ggtitle("control_trace"),
    density = ggplot2::ggplot() + ggplot2::ggtitle("control_density")
  )
  class(control_plots) <- c("mixgpd_fit_plots", "list")

  out <- list(treated = treated_plots, control = control_plots)
  class(out) <- c("dpmixgpd_causal_fit_plots", "list")
  out
}

make_mock_summary_qte <- function() {
  qte_obj <- make_mock_qte()
  out <- list(
    overall = list(
      n_pred = 5,
      n_quantiles = 3,
      quantiles = c(0.25, 0.5, 0.75),
      level = 0.95,
      interval = "credible",
      has_covariates = TRUE,
      ps_used = TRUE
    ),
    quantile_summary = data.frame(
      quantile = c(0.25, 0.5, 0.75),
      mean_qte = c(0.1, 0.2, 0.3),
      median_qte = c(0.1, 0.2, 0.3),
      min_qte = c(-0.1, 0, 0.1),
      max_qte = c(0.3, 0.4, 0.5),
      sd_qte = c(0.1, 0.1, 0.1)
    ),
    ci_summary = list(
      mean_width = 0.5,
      median_width = 0.5,
      min_width = 0.3,
      max_width = 0.7
    ),
    meta = list(
      backend = list(trt = "sb", con = "sb"),
      kernel = list(trt = "normal", con = "normal"),
      GPD = list(trt = FALSE, con = FALSE)
    ),
    object = qte_obj
  )
  class(out) <- "summary.dpmixgpd_qte"
  out
}

make_mock_summary_ate <- function() {
  ate_obj <- make_mock_ate()
  out <- list(
    overall = list(
      n_pred = 5,
      level = 0.95,
      interval = "credible",
      nsim_mean = 200,
      has_covariates = TRUE,
      ps_used = TRUE
    ),
    ate_stats = list(
      mean_ate = 0.5,
      median_ate = 0.45,
      min_ate = 0.1,
      max_ate = 0.9,
      sd_ate = 0.2
    ),
    ci_summary = list(
      mean_width = 0.5,
      median_width = 0.5,
      min_width = 0.3,
      max_width = 0.7
    ),
    meta = list(
      backend = list(trt = "sb", con = "sb"),
      kernel = list(trt = "normal", con = "normal"),
      GPD = list(trt = FALSE, con = FALSE)
    ),
    object = ate_obj
  )
  class(out) <- "summary.dpmixgpd_ate"
  out
}

make_mock_mixgpd_fitted_plots <- function() {
  out <- list(
    observed_fitted_plot = ggplot2::ggplot() + ggplot2::ggtitle("obs_fit"),
    residual_plot = ggplot2::ggplot() + ggplot2::ggtitle("residuals")
  )
  class(out) <- c("mixgpd_fitted_plots", "list")
  out
}

# ======================================================================
# print.mixgpd_params tests
# ======================================================================

test_that("print.mixgpd_params works with non-empty params", {
  params <- make_mock_mixgpd_params()
  expect_output(print_mixgpd_params(params), "Posterior mean parameters")
  expect_output(print_mixgpd_params(params), "alpha")
  expect_output(print_mixgpd_params(params), "beta_mu")
})

test_that("print.mixgpd_params works with empty params", {
  params <- make_mock_mixgpd_params(empty = TRUE)
  expect_output(print_mixgpd_params(params), "empty")
})

test_that("print.mixgpd_params respects digits argument", {
  params <- make_mock_mixgpd_params()
  expect_output(print_mixgpd_params(params, digits = 2), regexp = "Posterior mean parameters")
})

# ======================================================================
# print.mixgpd_params_pair tests
# ======================================================================

test_that("print.mixgpd_params_pair works", {
  params_pair <- make_mock_mixgpd_params_pair()
  expect_output(print_mixgpd_params_pair(params_pair), "Posterior mean parameters \\(causal\\)")
  expect_output(print_mixgpd_params_pair(params_pair), "treated")
  expect_output(print_mixgpd_params_pair(params_pair), "control")
})

# ======================================================================
# print.dpmixgpd_qte tests
# ======================================================================

test_that("print.dpmixgpd_qte works", {
  qte <- make_mock_qte()
  expect_output(print_dpmixgpd_qte(qte), "QTE \\(Quantile Treatment Effect\\)")
  expect_output(print_dpmixgpd_qte(qte), "Prediction points")
  expect_output(print_dpmixgpd_qte(qte), "Quantile grid")
})

test_that("print.dpmixgpd_qte respects max_rows", {
  qte <- make_mock_qte()
  expect_output(print_dpmixgpd_qte(qte, max_rows = 2), "more rows")
})

# ======================================================================
# print.dpmixgpd_ate tests
# ======================================================================

test_that("print.dpmixgpd_ate works", {
  ate <- make_mock_ate()
  expect_output(print_dpmixgpd_ate(ate), "ATE \\(Average Treatment Effect\\)")
  expect_output(print_dpmixgpd_ate(ate), "Prediction points")
})

test_that("print.dpmixgpd_ate respects max_rows", {
  ate <- make_mock_ate()
  expect_output(print_dpmixgpd_ate(ate, max_rows = 2), "more rows")
})

# ======================================================================
# summary.dpmixgpd_qte tests
# ======================================================================

test_that("summary.dpmixgpd_qte returns proper structure", {
  qte <- make_mock_qte()
  summ <- summary_dpmixgpd_qte(qte)
  expect_s3_class(summ, "summary.dpmixgpd_qte")
  expect_true("overall" %in% names(summ))
  expect_true("quantile_summary" %in% names(summ))
})

# ======================================================================
# summary.dpmixgpd_ate tests
# ======================================================================

test_that("summary.dpmixgpd_ate returns proper structure", {
  ate <- make_mock_ate()
  summ <- summary_dpmixgpd_ate(ate)
  expect_s3_class(summ, "summary.dpmixgpd_ate")
  expect_true("overall" %in% names(summ))
  expect_true("ate_stats" %in% names(summ))
})

# ======================================================================
# print.summary.dpmixgpd_qte tests
# ======================================================================

test_that("print.summary.dpmixgpd_qte works", {
  summ <- make_mock_summary_qte()
  expect_output(print_summary_dpmixgpd_qte(summ), "QTE Summary")
  expect_output(print_summary_dpmixgpd_qte(summ), "Prediction points")
  expect_output(print_summary_dpmixgpd_qte(summ), "Quantile grid")
  expect_output(print_summary_dpmixgpd_qte(summ), "Model specification")
})

test_that("print.summary.dpmixgpd_qte respects digits", {
  summ <- make_mock_summary_qte()
  expect_output(print_summary_dpmixgpd_qte(summ, digits = 2), "QTE Summary")
})

# ======================================================================
# print.summary.dpmixgpd_ate tests
# ======================================================================

test_that("print.summary.dpmixgpd_ate works", {
  summ <- make_mock_summary_ate()
  expect_output(print_summary_dpmixgpd_ate(summ), "ATE Summary")
  expect_output(print_summary_dpmixgpd_ate(summ), "Prediction points")
  expect_output(print_summary_dpmixgpd_ate(summ), "Model specification")
})

test_that("print.summary.dpmixgpd_ate respects digits", {
  summ <- make_mock_summary_ate()
  expect_output(print_summary_dpmixgpd_ate(summ, digits = 2), "ATE Summary")
})

# ======================================================================
# plot.mixgpd_predict tests
# ======================================================================

test_that("plot.mixgpd_predict works for quantile", {
  skip_if_not_installed("ggplot2")
  pred <- make_mock_mixgpd_predict("quantile")
  p <- plot_mixgpd_predict(pred)
  expect_true(inherits(p, "gg") || inherits(p, "ggplot"))
})

test_that("plot.mixgpd_predict works for sample", {
  skip_if_not_installed("ggplot2")
  pred <- make_mock_mixgpd_predict("sample")
  p <- plot_mixgpd_predict(pred)
  expect_true(inherits(p, "gg") || inherits(p, "ggplot"))
})

test_that("plot.mixgpd_predict works for mean", {
  skip_if_not_installed("ggplot2")
  pred <- make_mock_mixgpd_predict("mean")
  p <- plot_mixgpd_predict(pred)
  expect_true(inherits(p, "gg") || inherits(p, "ggplot"))
})

test_that("plot.mixgpd_predict works for density", {
  skip_if_not_installed("ggplot2")
  pred <- make_mock_mixgpd_predict("density")
  p <- plot_mixgpd_predict(pred)
  expect_true(inherits(p, "gg") || inherits(p, "ggplot"))
})

test_that("plot.mixgpd_predict works for survival", {
  skip_if_not_installed("ggplot2")
  pred <- make_mock_mixgpd_predict("survival")
  p <- plot_mixgpd_predict(pred)
  expect_true(inherits(p, "gg") || inherits(p, "ggplot"))
})

test_that("plot.mixgpd_predict works for location", {
  skip_if_not_installed("ggplot2")
  pred <- make_mock_mixgpd_predict("location")
  p <- plot_mixgpd_predict(pred)
  expect_true(inherits(p, "gg") || inherits(p, "ggplot") || is.list(p))
})

test_that("plot.mixgpd_predict errors on non-list input", {
  expect_error(plot_mixgpd_predict("not_a_list"), "must be a prediction object")
})

test_that("plot.mixgpd_predict errors on missing type", {
  pred <- list(fit = data.frame(x = 1))
  class(pred) <- "mixgpd_predict"
  expect_error(plot_mixgpd_predict(pred), "missing 'type'")
})

# ======================================================================
# print.dpmixgpd_causal_predict_plots tests
# ======================================================================

test_that("print.dpmixgpd_causal_predict_plots works", {
  skip_if_not_installed("ggplot2")
  plots <- make_mock_causal_predict_plots()
  expect_output(print_dpmixgpd_causal_predict_plots(plots), regexp = ".")
})

# ======================================================================
# print.mixgpd_predict_plots tests
# ======================================================================

test_that("print.mixgpd_predict_plots works", {
  skip_if_not_installed("ggplot2")
  p <- make_mock_mixgpd_predict_plots()
  # Should not error
  expect_silent(invisible(capture.output(print_mixgpd_predict_plots(p))))
})

# ======================================================================
# print.mixgpd_fit_plots tests
# ======================================================================

test_that("print.mixgpd_fit_plots works", {
  skip_if_not_installed("ggplot2")
  plots <- make_mock_mixgpd_fit_plots()
  expect_output(print_mixgpd_fit_plots(plots), "traceplot")
  expect_output(print_mixgpd_fit_plots(plots), "density")
})

# ======================================================================
# print.dpmixgpd_causal_fit_plots tests
# ======================================================================

test_that("print.dpmixgpd_causal_fit_plots works", {
  skip_if_not_installed("ggplot2")
  plots <- make_mock_causal_fit_plots()
  expect_output(print_dpmixgpd_causal_fit_plots(plots), "treated")
  expect_output(print_dpmixgpd_causal_fit_plots(plots), "control")
})

# ======================================================================
# plot.mixgpd_fitted tests
# ======================================================================

test_that("plot.mixgpd_fitted returns plot list", {
  skip_if_not_installed("ggplot2")
  fitted_obj <- make_mock_mixgpd_fitted()
  result <- plot_mixgpd_fitted(fitted_obj)
  expect_s3_class(result, "mixgpd_fitted_plots")
  expect_true("observed_fitted_plot" %in% names(result))
  expect_true("residual_plot" %in% names(result))
})

# ======================================================================
# print.mixgpd_fitted_plots tests
# ======================================================================

test_that("print.mixgpd_fitted_plots works", {
  skip_if_not_installed("ggplot2")
  plots <- make_mock_mixgpd_fitted_plots()
  # Should not error (print produces ggplot output but not text)
  expect_silent(invisible(capture.output(print_mixgpd_fitted_plots(plots))))
})

# ======================================================================
# plot.dpmixgpd_qte tests
# ======================================================================

test_that("plot.dpmixgpd_qte returns both plots by default", {
  skip_if_not_installed("ggplot2")
  qte <- make_mock_qte()
  result <- plot_dpmixgpd_qte(qte)
  expect_s3_class(result, "dpmixgpd_causal_predict_plots")
  expect_true("trt_control" %in% names(result))
  expect_true("treatment_effect" %in% names(result))
})

test_that("plot.dpmixgpd_qte effect type works", {
  skip_if_not_installed("ggplot2")
  qte <- make_mock_qte()
  result <- plot_dpmixgpd_qte(qte, type = "effect")
  expect_true(inherits(result, "gg") || inherits(result, "ggplot"))
})

test_that("plot.dpmixgpd_qte arms type works", {
  skip_if_not_installed("ggplot2")
  qte <- make_mock_qte()
  result <- plot_dpmixgpd_qte(qte, type = "arms")
  expect_true(inherits(result, "gg") || inherits(result, "ggplot"))
})

# ======================================================================
# plot.dpmixgpd_ate tests
# ======================================================================

test_that("plot.dpmixgpd_ate returns both plots by default", {
  skip_if_not_installed("ggplot2")
  ate <- make_mock_ate()
  result <- plot_dpmixgpd_ate(ate)
  expect_s3_class(result, "dpmixgpd_causal_predict_plots")
  expect_true("trt_control" %in% names(result))
  expect_true("treatment_effect" %in% names(result))
})

test_that("plot.dpmixgpd_ate effect type works", {
  skip_if_not_installed("ggplot2")
  ate <- make_mock_ate()
  result <- plot_dpmixgpd_ate(ate, type = "effect")
  expect_true(inherits(result, "gg") || inherits(result, "ggplot"))
})

test_that("plot.dpmixgpd_ate arms type works", {
  skip_if_not_installed("ggplot2")
  ate <- make_mock_ate()
  result <- plot_dpmixgpd_ate(ate, type = "arms")
  expect_true(inherits(result, "gg") || inherits(result, "ggplot"))
})

# ======================================================================
# Error handling
# ======================================================================

test_that("plot.dpmixgpd_qte errors on invalid object", {
  skip_if_not_installed("ggplot2")
  invalid <- list(x = 1)
  class(invalid) <- "dpmixgpd_qte"
  expect_error(plot_dpmixgpd_qte(invalid), "Invalid QTE object")
})

test_that("plot.dpmixgpd_ate errors on invalid object", {
  skip_if_not_installed("ggplot2")
  invalid <- list(x = 1)
  class(invalid) <- "dpmixgpd_ate"
  expect_error(plot_dpmixgpd_ate(invalid), "Invalid ATE object")
})

# ======================================================================
# Additional S3 method tests for bundles and fits
# ======================================================================

# Access more S3 methods
print_dpmixgpd_bundle <- DPmixGPD:::print.dpmixgpd_bundle
print_dpmixgpd_causal_bundle <- DPmixGPD:::print.dpmixgpd_causal_bundle
summary_dpmixgpd_bundle <- DPmixGPD:::summary.dpmixgpd_bundle
summary_dpmixgpd_causal_bundle <- DPmixGPD:::summary.dpmixgpd_causal_bundle
print_dpmixgpd_ps_bundle <- DPmixGPD:::print.dpmixgpd_ps_bundle
summary_dpmixgpd_ps_bundle <- DPmixGPD:::summary.dpmixgpd_ps_bundle
print_dpmixgpd_causal_fit <- DPmixGPD:::print.dpmixgpd_causal_fit
summary_dpmixgpd_causal_fit <- DPmixGPD:::summary.dpmixgpd_causal_fit
print_dpmixgpd_ps_fit <- DPmixGPD:::print.dpmixgpd_ps_fit
summary_dpmixgpd_ps_fit <- DPmixGPD:::summary.dpmixgpd_ps_fit
print_mixgpd_fit <- DPmixGPD:::print.mixgpd_fit
summary_mixgpd_fit <- DPmixGPD:::summary.mixgpd_fit
print_mixgpd_summary <- DPmixGPD:::print.mixgpd_summary

# Access exported functions
build_nimble_bundle <- DPmixGPD::build_nimble_bundle
build_causal_bundle <- DPmixGPD::build_causal_bundle

# ======================================================================
# print/summary.dpmixgpd_bundle tests
# ======================================================================

test_that("print.dpmixgpd_bundle works with basic bundle", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )
  expect_output(print_dpmixgpd_bundle(bundle), "DPmixGPD bundle")
  expect_output(print_dpmixgpd_bundle(bundle), "Stick-Breaking")
  expect_output(print_dpmixgpd_bundle(bundle), "Normal")
})

test_that("print.dpmixgpd_bundle shows code when requested", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )
  expect_output(print_dpmixgpd_bundle(bundle, code = TRUE), "Model code")
})

test_that("summary.dpmixgpd_bundle works", {
  set.seed(1)
  y <- abs(rnorm(20)) + 0.1
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    GPD = FALSE,
    components = 4
  )
  expect_output(summary_dpmixgpd_bundle(bundle), "DPmixGPD bundle summary")
  expect_output(summary_dpmixgpd_bundle(bundle), "Parameter specification")
  expect_output(summary_dpmixgpd_bundle(bundle), "Monitors")
})

# ======================================================================
# print/summary.dpmixgpd_causal_bundle tests
# ======================================================================

test_that("print.dpmixgpd_causal_bundle works", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y, X = X, T = T,
    backend = "sb", kernel = "normal",
    GPD = FALSE, components = 4
  )
  expect_output(print_dpmixgpd_causal_bundle(bundle), "DPmixGPD causal bundle")
  expect_output(print_dpmixgpd_causal_bundle(bundle), "Outcome.*treated")
  expect_output(print_dpmixgpd_causal_bundle(bundle), "Outcome.*control")
})

test_that("print.dpmixgpd_causal_bundle shows PS info for observational", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y, X = X, T = T,
    backend = "sb", kernel = "normal",
    GPD = FALSE, components = 4, PS = "logit"
  )
  expect_output(print_dpmixgpd_causal_bundle(bundle), "PS model.*logit")
})

test_that("summary.dpmixgpd_causal_bundle works", {
  set.seed(1)
  y <- rnorm(20)
  X <- matrix(rnorm(40), ncol = 2)
  T <- c(rep(0, 10), rep(1, 10))

  bundle <- build_causal_bundle(
    y = y, X = X, T = T,
    backend = "sb", kernel = "normal",
    GPD = FALSE, components = 4
  )
  expect_output(summary_dpmixgpd_causal_bundle(bundle), "DPmixGPD causal bundle summary")
})

# ======================================================================
# Mock PS bundle tests
# ======================================================================

make_mock_ps_bundle <- function() {
  out <- list(
    spec = list(
      meta = list(
        type = "ps_logit",
        include_intercept = TRUE
      )
    ),
    code = quote({
      beta[1] ~ dnorm(0, 1)
    })
  )
  class(out) <- "dpmixgpd_ps_bundle"
  out
}

test_that("print.dpmixgpd_ps_bundle works", {
  bundle <- make_mock_ps_bundle()
  expect_output(print_dpmixgpd_ps_bundle(bundle), "PS bundle")
  expect_output(print_dpmixgpd_ps_bundle(bundle), "model: logit")
})

test_that("print.dpmixgpd_ps_bundle shows code when requested", {
  bundle <- make_mock_ps_bundle()
  expect_output(print_dpmixgpd_ps_bundle(bundle, code = TRUE), "beta")
})

test_that("summary.dpmixgpd_ps_bundle works", {
  bundle <- make_mock_ps_bundle()
  expect_output(summary_dpmixgpd_ps_bundle(bundle), "PS bundle")
})

# ======================================================================
# Mock PS fit tests
# ======================================================================

make_mock_ps_fit <- function() {
  out <- list(
    bundle = make_mock_ps_bundle()
  )
  class(out) <- "dpmixgpd_ps_fit"
  out
}

test_that("print.dpmixgpd_ps_fit works", {
  fit <- make_mock_ps_fit()
  expect_output(print_dpmixgpd_ps_fit(fit), "DPmixGPD PS fit")
  expect_output(print_dpmixgpd_ps_fit(fit), "model: logit")
})

test_that("summary.dpmixgpd_ps_fit works", {
  fit <- make_mock_ps_fit()
  expect_output(summary_dpmixgpd_ps_fit(fit), "DPmixGPD PS fit")
})

# ======================================================================
# Mock causal fit tests
# ======================================================================

make_mock_mixgpd_fit <- function() {
  out <- list(
    spec = list(
      meta = list(
        backend = "sb",
        kernel = "normal",
        GPD = FALSE,
        N = 50,
        components = 4
      ),
      dispatch = list(backend = "sb")
    ),
    data = list(y = rnorm(50)),
    mcmc = list(niter = 100, nburnin = 20, thin = 1, nchains = 1),
    epsilon = 0.025
  )
  class(out) <- "mixgpd_fit"
  out
}

make_mock_causal_fit <- function() {
  out <- list(
    bundle = list(
      meta = list(
        backend = list(trt = "sb", con = "sb"),
        kernel = list(trt = "normal", con = "normal"),
        GPD = list(trt = FALSE, con = FALSE),
        ps = list(enabled = FALSE, model_type = FALSE)
      ),
      data = list(X = NULL)
    ),
    outcome_fit = list(
      trt = make_mock_mixgpd_fit(),
      con = make_mock_mixgpd_fit()
    ),
    ps_fit = NULL
  )
  class(out) <- "dpmixgpd_causal_fit"
  out
}

test_that("print.dpmixgpd_causal_fit works", {
  fit <- make_mock_causal_fit()
  expect_output(print_dpmixgpd_causal_fit(fit), "DPmixGPD causal fit")
  expect_output(print_dpmixgpd_causal_fit(fit), "Outcome.*treated")
  expect_output(print_dpmixgpd_causal_fit(fit), "Outcome.*control")
})

test_that("print.dpmixgpd_causal_fit shows PS info", {
  fit <- make_mock_causal_fit()
  expect_output(print_dpmixgpd_causal_fit(fit), "PS model")
})

# ======================================================================
# Mock mixgpd_fit print tests
# ======================================================================

test_that("print.mixgpd_fit works", {
  fit <- make_mock_mixgpd_fit()
  expect_output(print_mixgpd_fit(fit), "MixGPD fit")
  expect_output(print_mixgpd_fit(fit), "Stick-Breaking")
  expect_output(print_mixgpd_fit(fit), "Normal")
})

# ======================================================================
# Mock summary output tests
# ======================================================================

make_mock_mixgpd_summary <- function() {
  out <- list(
    model = list(
      backend = "sb",
      kernel = "normal",
      gpd = FALSE,
      n = 50,
      components = 4,
      epsilon = 0.025,
      truncation = list(Kt = 2)
    ),
    waic = list(WAIC = 123.4, lppd = -60.1, pWAIC = 1.6),
    table = data.frame(
      parameter = c("w[1]", "w[2]", "mean[1]", "mean[2]"),
      mean = c(0.6, 0.4, 1.5, 2.5),
      sd = c(0.1, 0.1, 0.2, 0.3)
    )
  )
  class(out) <- "mixgpd_summary"
  out
}

test_that("print.mixgpd_summary works", {
  summ <- make_mock_mixgpd_summary()
  expect_output(print_mixgpd_summary(summ), "MixGPD summary")
  expect_output(print_mixgpd_summary(summ), "Stick-Breaking")
  expect_output(print_mixgpd_summary(summ), "Normal")
})
