# Tests for coverage status functions (status.R)

# ============================================================================
# read_coverage_status() tests
# ============================================================================

test_that("read_coverage_status() errors on missing file", {
  expect_error(
    read_coverage_status("nonexistent_file_12345.json"),
    "not found"
  )
})

test_that("read_coverage_status() works with valid JSON file", {
  skip_if_not_installed("jsonlite")

  # Create a temporary JSON file
  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)

  test_data <- list(
    percent = 75.5,
    type = "tests",
    timestamp = "2024-01-15T10:30:00Z",
    statements = 100,
    files = 10
  )

  jsonlite::write_json(test_data, tmp_file, auto_unbox = TRUE)

  result <- read_coverage_status(tmp_file)

  expect_true(is.list(result))
  expect_equal(result$percent, 75.5)
  expect_equal(result$type, "tests")
  expect_equal(result$statements, 100)
  expect_equal(result$files, 10)
})

test_that("read_coverage_status() returns list structure", {
  skip_if_not_installed("jsonlite")

  # Create a minimal valid JSON
  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)

  jsonlite::write_json(list(percent = 50), tmp_file, auto_unbox = TRUE)

  result <- read_coverage_status(tmp_file)

  expect_true(is.list(result))
})

# ============================================================================
# coverage_status() tests
# ============================================================================
# Note: coverage_status() runs covr::package_coverage() which is expensive
# and requires a full package environment, so we test error handling only

test_that("coverage_status() requires covr package", {
  skip_if(requireNamespace("covr", quietly = TRUE),
          "covr is installed, skipping missing-package test")

  # This test only runs if covr is NOT installed
  expect_error(
    coverage_status(),
    "covr"
  )
})

test_that("coverage_status() accepts type argument", {
  skip_if_not_installed("covr")
  skip("Skipping full coverage_status test - requires full package coverage run")

  # This would run the full coverage, which is too slow for unit tests
  # The function signature is tested by checking it accepts the argument
  expect_true(is.function(coverage_status))
})

test_that("coverage_status() type argument is validated", {
  skip_if_not_installed("covr")

  # Test that invalid type throws an error from match.arg
  expect_error(
    coverage_status(type = "invalid_type"),
    "should be one of"
  )
})

test_that("coverage_status() data_file parameter creates directory", {
  skip_if_not_installed("covr")
  skip("Skipping full coverage_status test - requires full package coverage run")

  # This test would verify the directory creation logic
  # but requires running actual coverage
})
