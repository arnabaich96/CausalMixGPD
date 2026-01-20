# Tests for kernel_support_table() function (kernel_support.R)

test_that("kernel_support_table() returns a data frame", {
  result <- kernel_support_table()

  expect_true(is.data.frame(result))
})

test_that("kernel_support_table() has correct columns", {
  result <- kernel_support_table()

  expected_cols <- c("kernel", "gpd", "covariates", "sb", "crp")
  expect_equal(names(result), expected_cols)
})

test_that("kernel_support_table() includes all 7 kernels", {
  result <- kernel_support_table()

  expected_kernels <- c("normal", "lognormal", "invgauss", "gamma", "laplace", "amoroso", "cauchy")
  expect_equal(nrow(result), 7L)
  expect_true(all(expected_kernels %in% result$kernel))
})

test_that("kernel_support_table(round=TRUE) produces checkmark/cross symbols", {
  result <- kernel_support_table(round = TRUE)

  # Check that gpd, covariates, sb, crp columns contain Unicode symbols
  checkmark <- "\u2714"
  cross <- "\u274C"

  # All values in these columns should be one of the symbols
  for (col in c("gpd", "covariates", "sb", "crp")) {
    expect_true(all(result[[col]] %in% c(checkmark, cross)),
                info = paste0("column '", col, "' should contain only checkmark/cross"))
  }
})

test_that("kernel_support_table(round=FALSE) produces logical values", {
  result <- kernel_support_table(round = FALSE)

  # Check that gpd, covariates, sb, crp columns are logical
  for (col in c("gpd", "covariates", "sb", "crp")) {
    expect_true(is.logical(result[[col]]),
                info = paste0("column '", col, "' should be logical"))
  }
})

test_that("Cauchy kernel does not support GPD", {
  result <- kernel_support_table(round = FALSE)

  cauchy_row <- result[result$kernel == "cauchy", ]
  expect_false(cauchy_row$gpd)
})

test_that("All kernels support both sb and crp backends", {
  result <- kernel_support_table(round = FALSE)

  expect_true(all(result$sb))
  expect_true(all(result$crp))
})

test_that("All kernels support covariates", {
  result <- kernel_support_table(round = FALSE)

  expect_true(all(result$covariates))
})

test_that("Most kernels support GPD (except Cauchy)", {
  result <- kernel_support_table(round = FALSE)

  gpd_kernels <- result[result$kernel != "cauchy", ]
  expect_true(all(gpd_kernels$gpd))
})

test_that("kernel_support_table() kernel column is character", {
  result <- kernel_support_table()

  expect_true(is.character(result$kernel))
})
