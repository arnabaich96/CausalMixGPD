# Tests for visualization helper functions (06-visualization-helpers.R)
# These are internal functions (@noRd) but count for coverage

# ============================================================================
# .plot_quantile_pred() tests
# ============================================================================

test_that(".plot_quantile_pred() works without id column", {
  skip_if_not_installed("ggplot2")

  # Create mock prediction object
  mock_pred <- list(
    fit = data.frame(
      index = c(0.25, 0.50, 0.75, 0.95),
      estimate = c(1.0, 2.0, 3.5, 6.0),
      lower = c(0.8, 1.7, 3.0, 5.0),
      upper = c(1.2, 2.3, 4.0, 7.0)
    )
  )

  # Access the internal function
  plot_fn <- DPmixGPD:::.plot_quantile_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_quantile_pred() works with id column", {
  skip_if_not_installed("ggplot2")

  # Create mock prediction object with id
  mock_pred <- list(
    fit = data.frame(
      id = rep(1:2, each = 3),
      index = rep(c(0.25, 0.50, 0.75), 2),
      estimate = c(1.0, 2.0, 3.0, 1.5, 2.5, 3.5),
      lower = c(0.8, 1.7, 2.5, 1.2, 2.2, 3.0),
      upper = c(1.2, 2.3, 3.5, 1.8, 2.8, 4.0)
    )
  )

  plot_fn <- DPmixGPD:::.plot_quantile_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_quantile_pred() errors on non-data.frame fit", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(fit = c(1, 2, 3))

  plot_fn <- DPmixGPD:::.plot_quantile_pred

  expect_error(plot_fn(mock_pred), "data frame")
})

# ============================================================================
# .plot_sample_pred() tests
# ============================================================================

test_that(".plot_sample_pred() works with numeric vector", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(
    fit = rnorm(100, mean = 5, sd = 1)
  )

  plot_fn <- DPmixGPD:::.plot_sample_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_sample_pred() errors on non-numeric fit", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(fit = data.frame(x = 1:10))

  plot_fn <- DPmixGPD:::.plot_sample_pred

  expect_error(plot_fn(mock_pred), "numeric vector")
})

# ============================================================================
# .plot_mean_pred() tests
# ============================================================================

test_that(".plot_mean_pred() works with draws available", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(
    fit = data.frame(
      estimate = 5.0,
      lower = 4.5,
      upper = 5.5
    ),
    draws = rnorm(200, mean = 5, sd = 0.3)
  )

  plot_fn <- DPmixGPD:::.plot_mean_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_mean_pred() works without draws", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(
    fit = data.frame(
      estimate = 5.0,
      lower = 4.5,
      upper = 5.5
    ),
    draws = NULL
  )

  plot_fn <- DPmixGPD:::.plot_mean_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_mean_pred() works with vector fit (old format)", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(
    fit = c(5.0, 4.8, 5.2),
    draws = rnorm(100, mean = 5, sd = 0.2)
  )

  plot_fn <- DPmixGPD:::.plot_mean_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

# ============================================================================
# .plot_density_pred() tests
# ============================================================================

test_that(".plot_density_pred() works with data frame format", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(
    fit = data.frame(
      y = seq(0, 10, length.out = 50),
      density = dnorm(seq(0, 10, length.out = 50), mean = 5, sd = 1)
    )
  )

  plot_fn <- DPmixGPD:::.plot_density_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_density_pred() works with vector format", {
  skip_if_not_installed("ggplot2")

  grid <- seq(0, 10, length.out = 50)
  mock_pred <- list(
    fit = dnorm(grid, mean = 5, sd = 1),
    grid = grid
  )

  plot_fn <- DPmixGPD:::.plot_density_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_density_pred() works with id column", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(
    fit = data.frame(
      id = rep(1:2, each = 25),
      y = rep(seq(0, 10, length.out = 25), 2),
      density = c(
        dnorm(seq(0, 10, length.out = 25), mean = 4, sd = 1),
        dnorm(seq(0, 10, length.out = 25), mean = 6, sd = 1)
      )
    )
  )

  plot_fn <- DPmixGPD:::.plot_density_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

# ============================================================================
# .plot_survival_pred() tests
# ============================================================================

test_that(".plot_survival_pred() works with data frame format", {
  skip_if_not_installed("ggplot2")

  y_vals <- seq(0, 10, length.out = 30)
  mock_pred <- list(
    fit = data.frame(
      y = y_vals,
      survival = 1 - pnorm(y_vals, mean = 5, sd = 2)
    )
  )

  plot_fn <- DPmixGPD:::.plot_survival_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_survival_pred() works with vector format", {
  skip_if_not_installed("ggplot2")

  grid <- seq(0, 10, length.out = 30)
  mock_pred <- list(
    fit = 1 - pnorm(grid, mean = 5, sd = 2),
    grid = grid
  )

  plot_fn <- DPmixGPD:::.plot_survival_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_survival_pred() works with id column", {
  skip_if_not_installed("ggplot2")

  y_vals <- seq(0, 10, length.out = 20)
  mock_pred <- list(
    fit = data.frame(
      id = rep(1:2, each = 20),
      y = rep(y_vals, 2),
      survival = c(
        1 - pnorm(y_vals, mean = 4, sd = 1.5),
        1 - pnorm(y_vals, mean = 6, sd = 1.5)
      )
    )
  )

  plot_fn <- DPmixGPD:::.plot_survival_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

# ============================================================================
# .plot_location_pred() tests
# ============================================================================

test_that(".plot_location_pred() works without id column", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(
    fit = data.frame(
      mean = 5.0,
      mean_lower = 4.5,
      mean_upper = 5.5,
      median = 4.8,
      median_lower = 4.3,
      median_upper = 5.3
    )
  )

  plot_fn <- DPmixGPD:::.plot_location_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_location_pred() works with id column", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(
    fit = data.frame(
      id = 1:3,
      mean = c(5.0, 5.5, 6.0),
      mean_lower = c(4.5, 5.0, 5.5),
      mean_upper = c(5.5, 6.0, 6.5),
      median = c(4.8, 5.3, 5.8),
      median_lower = c(4.3, 4.8, 5.3),
      median_upper = c(5.3, 5.8, 6.3)
    )
  )

  plot_fn <- DPmixGPD:::.plot_location_pred

  result <- plot_fn(mock_pred)

  expect_true(inherits(result, "ggplot"))
})

test_that(".plot_location_pred() errors on non-data.frame fit", {
  skip_if_not_installed("ggplot2")

  mock_pred <- list(fit = c(1, 2, 3))

  plot_fn <- DPmixGPD:::.plot_location_pred

  expect_error(plot_fn(mock_pred), "data frame")
})
