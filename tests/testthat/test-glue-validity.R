# test-glue-validity.R
# CI-safe smoke test for glue diagnostics.

test_that("check_glue_validity passes on fixture (short grid, few draws)", {
  testthat::skip_on_cran()
  skip_if_not_test_level("ci")

  fit <- .load_fixture("fit_gpd_small.rds")
  small_grid <- seq(0.1, 5, length.out = 20)

  out <- check_glue_validity(fit, n_draws = 5L, grid = small_grid, check_continuity = FALSE)
  expect_true(is.list(out))
  expect_true(is.list(out$pass))
  expect_true(isTRUE(out$pass$cdf_range))
  expect_true(isTRUE(out$pass$cdf_monotone))
  expect_true(isTRUE(out$pass$density_nonneg))
})
