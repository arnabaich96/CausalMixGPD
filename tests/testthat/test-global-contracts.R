# Tests for global contract functions (00-global-contracts.R)

# ============================================================================
# is_allowed_kernel() tests
# ============================================================================

test_that("is_allowed_kernel() returns TRUE for valid kernels", {
  is_allowed_kernel <- DPmixGPD:::is_allowed_kernel

  expect_true(is_allowed_kernel("normal"))
  expect_true(is_allowed_kernel("gamma"))
  expect_true(is_allowed_kernel("lognormal"))
  expect_true(is_allowed_kernel("invgauss"))
  expect_true(is_allowed_kernel("laplace"))
  expect_true(is_allowed_kernel("cauchy"))
  expect_true(is_allowed_kernel("amoroso"))
})

test_that("is_allowed_kernel() returns FALSE for invalid kernels", {
  is_allowed_kernel <- DPmixGPD:::is_allowed_kernel

  expect_false(is_allowed_kernel("invalid"))
  expect_false(is_allowed_kernel("weibull"))
  expect_false(is_allowed_kernel("exponential"))
  expect_false(is_allowed_kernel(""))
  expect_false(is_allowed_kernel("Normal"))  # Case sensitive
})

# ============================================================================
# check_gpd_contract() tests
# ============================================================================

test_that("check_gpd_contract() errors when GPD=TRUE with cauchy kernel", {
  check_gpd_contract <- DPmixGPD:::check_gpd_contract

  expect_error(
    check_gpd_contract(GPD = TRUE, kernel = "cauchy"),
    "Cauchy kernels are never paired with GPD"
  )
})

test_that("check_gpd_contract() returns invisibly for valid combinations", {
  check_gpd_contract <- DPmixGPD:::check_gpd_contract

  # GPD = TRUE with non-cauchy kernels should work
  expect_invisible(check_gpd_contract(GPD = TRUE, kernel = "normal"))
  expect_invisible(check_gpd_contract(GPD = TRUE, kernel = "gamma"))
  expect_invisible(check_gpd_contract(GPD = TRUE, kernel = "lognormal"))
  expect_invisible(check_gpd_contract(GPD = TRUE, kernel = "invgauss"))
  expect_invisible(check_gpd_contract(GPD = TRUE, kernel = "laplace"))
  expect_invisible(check_gpd_contract(GPD = TRUE, kernel = "amoroso"))

  # GPD = FALSE should always work
  expect_invisible(check_gpd_contract(GPD = FALSE, kernel = "cauchy"))
  expect_invisible(check_gpd_contract(GPD = FALSE, kernel = "normal"))
})

test_that("check_gpd_contract() returns NULL invisibly", {
  check_gpd_contract <- DPmixGPD:::check_gpd_contract

  result <- check_gpd_contract(GPD = TRUE, kernel = "normal")
  expect_null(result)
})

# ============================================================================
# allowed_* constant tests
# ============================================================================

test_that("allowed_backends constant is correct", {
  allowed_backends <- DPmixGPD:::allowed_backends

  expect_equal(allowed_backends, c("crp", "sb"))
})

test_that("allowed_kernels constant contains all 7 kernels", {
  allowed_kernels <- DPmixGPD:::allowed_kernels

  expected <- c("gamma", "lognormal", "invgauss", "normal", "laplace", "cauchy", "amoroso")
  expect_equal(sort(allowed_kernels), sort(expected))
})

test_that("positive_support_kernels constant is correct", {
  positive_support_kernels <- DPmixGPD:::positive_support_kernels

  expect_true("gamma" %in% positive_support_kernels)
  expect_true("lognormal" %in% positive_support_kernels)
  expect_true("invgauss" %in% positive_support_kernels)
  expect_true("amoroso" %in% positive_support_kernels)
  expect_false("normal" %in% positive_support_kernels)
  expect_false("laplace" %in% positive_support_kernels)
  expect_false("cauchy" %in% positive_support_kernels)
})

test_that("real_support_kernels constant is correct", {
  real_support_kernels <- DPmixGPD:::real_support_kernels

  expect_true("normal" %in% real_support_kernels)
  expect_true("laplace" %in% real_support_kernels)
  expect_true("cauchy" %in% real_support_kernels)
  expect_false("gamma" %in% real_support_kernels)
  expect_false("lognormal" %in% real_support_kernels)
  expect_false("invgauss" %in% real_support_kernels)
  expect_false("amoroso" %in% real_support_kernels)
})
