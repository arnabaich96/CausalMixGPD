# tests/testthat/test-visualization-helpers.R
# Unit tests for internal visualization helper functions (06-visualization-helpers.R)

# Access internal helpers using :::
.plot_quantile_pred <- CausalMixGPD:::.plot_quantile_pred
.plot_sample_pred <- CausalMixGPD:::.plot_sample_pred
.plot_mean_pred <- CausalMixGPD:::.plot_mean_pred
.plot_density_pred <- CausalMixGPD:::.plot_density_pred
.plot_survival_pred <- CausalMixGPD:::.plot_survival_pred
.plot_location_pred <- CausalMixGPD:::.plot_location_pred
.plot_palette <- CausalMixGPD:::.plot_palette
.plot_theme <- CausalMixGPD:::.plot_theme

# ======================================================================
# .plot_palette tests
# ======================================================================

test_that(".plot_palette returns correct number of colors", {
  expect_length(.plot_palette(3), 3)
  expect_length(.plot_palette(8), 8)
  expect_length(.plot_palette(1), 1)
})

test_that(".plot_palette recycles when n > 8", {
  pal <- .plot_palette(12)
  expect_length(pal, 12)
  # First 8 should repeat in positions 9-12
  expect_equal(pal[9], pal[1])
  expect_equal(pal[10], pal[2])
})

test_that(".plot_palette returns valid hex colors", {
  pal <- .plot_palette(8)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that(".plot_palette handles NULL input", {
  pal <- .plot_palette(NULL)
  expect_length(pal, 8)  # default length
})

# ======================================================================
# .plot_theme tests
# ======================================================================

test_that(".plot_theme returns a ggplot theme", {
  skip_if_not_installed("ggplot2")
  theme <- .plot_theme()
  expect_s3_class(theme, "theme")
})

# ======================================================================
# .plot_quantile_pred tests
# ======================================================================

test_that(".plot_quantile_pred works with data.frame fit", {
  skip_if_not_installed("ggplot2")
  pred <- list(
    fit = data.frame(
      index = c(0.25, 0.5, 0.75),
      estimate = c(1, 2, 3),
      lower = c(0.5, 1.5, 2.5),
      upper = c(1.5, 2.5, 3.5)
    )
  )
  p <- .plot_quantile_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_quantile_pred works with id column", {
  skip_if_not_installed("ggplot2")
  pred <- list(
    fit = data.frame(
      id = rep(1:2, each = 3),
      index = rep(c(0.25, 0.5, 0.75), 2),
      estimate = rnorm(6),
      lower = rnorm(6) - 0.5,
      upper = rnorm(6) + 0.5
    )
  )
  p <- .plot_quantile_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_quantile_pred errors on non-data.frame fit", {
  pred <- list(fit = c(1, 2, 3))
  expect_error(.plot_quantile_pred(pred), "data frame")
})

# ======================================================================
# .plot_sample_pred tests
# ======================================================================

test_that(".plot_sample_pred works with numeric samples", {
  skip_if_not_installed("ggplot2")
  pred <- list(fit = rnorm(100))
  p <- .plot_sample_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_sample_pred errors on non-numeric", {
  pred <- list(fit = data.frame(x = 1:5))
  expect_error(.plot_sample_pred(pred), "numeric vector")
})

# ======================================================================
# .plot_mean_pred tests
# ======================================================================

test_that(".plot_mean_pred works with data.frame fit and draws", {
  skip_if_not_installed("ggplot2")
  pred <- list(
    fit = data.frame(estimate = 5.0, lower = 4.5, upper = 5.5),
    draws = rnorm(100, mean = 5)
  )
  p <- .plot_mean_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_mean_pred works with data.frame fit without draws", {
  skip_if_not_installed("ggplot2")
  pred <- list(
    fit = data.frame(estimate = 5.0, lower = 4.5, upper = 5.5),
    draws = NULL
  )
  p <- .plot_mean_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_mean_pred works with numeric fit (old format)", {
  skip_if_not_installed("ggplot2")
  pred <- list(
    fit = c(4.5, 5.0, 5.5),
    draws = rnorm(100, mean = 5)
  )
  p <- .plot_mean_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_mean_pred works without CI bounds", {
  skip_if_not_installed("ggplot2")
  pred <- list(
    fit = data.frame(estimate = 5.0),
    draws = rnorm(100, mean = 5)
  )
  p <- .plot_mean_pred(pred)
  expect_s3_class(p, "gg")
})

# ======================================================================
# .plot_density_pred tests
# ======================================================================

test_that(".plot_density_pred works with data.frame fit", {
  skip_if_not_installed("ggplot2")
  y_grid <- seq(0, 5, length.out = 20)
  pred <- list(
    fit = data.frame(
      y = y_grid,
      density = dnorm(y_grid, mean = 2.5)
    )
  )
  p <- .plot_density_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_density_pred works with id column", {
  skip_if_not_installed("ggplot2")
  y_grid <- seq(0, 5, length.out = 10)
  pred <- list(
    fit = data.frame(
      id = rep(1:2, each = 10),
      y = rep(y_grid, 2),
      density = c(dnorm(y_grid, 2), dnorm(y_grid, 3))
    )
  )
  p <- .plot_density_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_density_pred works with numeric vector fit", {
  skip_if_not_installed("ggplot2")
  y_grid <- seq(0, 5, length.out = 20)
  pred <- list(
    fit = dnorm(y_grid, mean = 2.5),
    grid = y_grid
  )
  p <- .plot_density_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_density_pred handles grid column name", {
  skip_if_not_installed("ggplot2")
  pred <- list(
    fit = data.frame(
      grid = seq(0, 5, length.out = 10),
      density = dnorm(seq(0, 5, length.out = 10), mean = 2.5)
    )
  )
  p <- .plot_density_pred(pred)
  expect_s3_class(p, "gg")
})

# ======================================================================
# .plot_survival_pred tests
# ======================================================================

test_that(".plot_survival_pred works with data.frame fit", {
  skip_if_not_installed("ggplot2")
  y_grid <- seq(0, 5, length.out = 20)
  pred <- list(
    fit = data.frame(
      y = y_grid,
      survival = 1 - pnorm(y_grid, mean = 2.5)
    )
  )
  p <- .plot_survival_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_survival_pred works with id column", {
  skip_if_not_installed("ggplot2")
  y_grid <- seq(0, 5, length.out = 10)
  pred <- list(
    fit = data.frame(
      id = rep(1:2, each = 10),
      y = rep(y_grid, 2),
      survival = c(1 - pnorm(y_grid, 2), 1 - pnorm(y_grid, 3))
    )
  )
  p <- .plot_survival_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_survival_pred works with numeric vector fit", {
  skip_if_not_installed("ggplot2")
  y_grid <- seq(0, 5, length.out = 20)
  pred <- list(
    fit = 1 - pnorm(y_grid, mean = 2.5),
    grid = y_grid
  )
  p <- .plot_survival_pred(pred)
  expect_s3_class(p, "gg")
})

# ======================================================================
# .plot_location_pred tests
# ======================================================================

test_that(".plot_location_pred works with single row (no id)", {
  skip_if_not_installed("ggplot2")
  pred <- list(
    fit = data.frame(
      mean = 5.0, mean_lower = 4.5, mean_upper = 5.5,
      median = 4.9, median_lower = 4.4, median_upper = 5.4
    )
  )
  p <- .plot_location_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_location_pred works with id column", {
  skip_if_not_installed("ggplot2")
  pred <- list(
    fit = data.frame(
      id = 1:3,
      mean = c(5.0, 5.5, 6.0),
      mean_lower = c(4.5, 5.0, 5.5),
      mean_upper = c(5.5, 6.0, 6.5),
      median = c(4.9, 5.4, 5.9),
      median_lower = c(4.4, 4.9, 5.4),
      median_upper = c(5.4, 5.9, 6.4)
    )
  )
  p <- .plot_location_pred(pred)
  expect_s3_class(p, "gg")
})

test_that(".plot_location_pred errors on non-data.frame fit", {
  pred <- list(fit = c(1, 2, 3))
  expect_error(.plot_location_pred(pred), "data frame")
})
