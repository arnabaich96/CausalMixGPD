# CausalMixGPD Test Suite Documentation

This document describes the testing infrastructure for the CausalMixGPD package.

## Test Tier System

The package uses a three-tier testing system to balance thoroughness with speed.
Tests are controlled by the `DPMIXGPD_TEST_LEVEL` environment variable.

| Level  | Description                          | When to Use              | Approx. Time   |
|--------|--------------------------------------|--------------------------|----------------|
| `cran` | Fast unit tests, no MCMC compilation | Default, quick feedback  | ~1-2 minutes   |
| `ci`   | Integration tests with MCMC          | CI pipelines, PR checks  | ~10-20 minutes |
| `full` | Exhaustive kernel/backend grid       | Pre-release validation   | ~1+ hours      |

### Why Tiers?

MCMC tests require NIMBLE C++ compilation, which is computationally expensive.
Running all tests during every development cycle would be impractical. The
tiered system allows:

- **Fast feedback** during development (cran level)
- **Thorough testing** in CI pipelines (ci level)
- **Exhaustive testing** before releases (full level)

## Running Tests

### Basic Commands

```r
# Run tests at default (cran) level - fast unit tests only
testthat::test_local()
```

### Running at Different Levels

```r
# Run at CI level (includes MCMC integration tests)
Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci")
testthat::test_local()

# Run exhaustive tests (all kernel/backend combinations)
Sys.setenv(DPMIXGPD_TEST_LEVEL = "full")
testthat::test_local()

# Reset to default
Sys.setenv(DPMIXGPD_TEST_LEVEL = "cran")
```

### Running Specific Tests

```r
# Run the dedicated coverage-only test file
Sys.setenv(DPMIXGPD_CI_COVERAGE_ONLY = "1")
testthat::test_file("tests/testthat/test-ci-level-only.R")

# Run the merged suite entrypoints
testthat::test_file("tests/testthat/test-unit.R")
testthat::test_file("tests/testthat/test-integration.R")

# Run tests matching a suite or feature pattern
testthat::test_local(filter = "unit")
testthat::test_local(filter = "integration")
testthat::test_local(filter = "ci-level-only")
testthat::test_local(filter = "causal")

# Run with verbose output
Sys.setenv(DPMIXGPD_TEST_VERBOSE = "1")
testthat::test_local()
```

### Using the Test Cache

For expensive tests, a caching system is available to reuse MCMC results:

```r
# Enable caching (speeds up repeated test runs)
Sys.setenv(DPMIXGPD_USE_CACHE = "1")
testthat::test_local()

# Disable caching
Sys.setenv(DPMIXGPD_USE_CACHE = "0")

# Custom cache directory
Sys.setenv(DPMIXGPD_CACHE_DIR = "/path/to/cache")
```

Cache files are stored in `tests/testthat/_cache/` by default.

## Test File Organization

Tests are currently organized around three runnable suite entrypoints. Larger
blocks that would make those files unwieldy live under `fragments/` and are
sourced from the matching suite file.

```text
tests/testthat/
- helper-00-levels.R              # Tier system functions and skip helpers
- helper-01-fixtures.R            # Test fixtures and data utilities
- helper-02-cache.R               # MCMC result caching infrastructure
- helper-03-predict-helpers.R     # Prediction test utilities
- setup.R                         # Global test setup, runs before all tests
- test-unit.R                     # Fast, deterministic tests (cran level)
- test-integration.R              # CI/full integration and performance suites
- test-ci-level-only.R            # Dedicated coverage-only CI suite
- fragments/unit/                 # Non-runnable unit fragments sourced by test-unit.R
- fragments/integration/          # Non-runnable integration fragments sourced by test-integration.R
```

### Helper Files

| File                          | Purpose                                  |
|-------------------------------|------------------------------------------|
| `setup.R`                     | Global test setup, runs before all tests |
| `helper-00-levels.R`          | Tier system functions and skip helpers   |
| `helper-01-fixtures.R`        | Test fixtures and data utilities         |
| `helper-02-cache.R`           | MCMC result caching infrastructure       |
| `helper-03-predict-helpers.R` | Prediction test utilities                |

**Note:** Helper files are loaded alphabetically by testthat before any test
runs.

### Test Files

#### `test-unit.R` - Fast Unit Tests (Tier A / cran level)

Fast, deterministic tests with no MCMC compilation (~1-2 minutes total).

- `test-unit.R`: Distribution, kernel, utils, S3, plotting, contract, progress,
  and light cluster helper coverage.

#### `test-ci-level-only.R` - Dedicated Coverage Suite

Coverage runs execute this dedicated coverage-only suite. This file is still
skipped in ordinary `testthat` runs unless `DPMIXGPD_CI_COVERAGE_ONLY=1`.

- `test-ci-level-only.R`: Distribution wrappers, representative workflows,
  wrapper entrypoints, and direct helper/S3 smoke coverage.

#### `test-integration.R` - Integration Tests (Tier B / ci level)

Heavier integration tests with MCMC, testing more combinatorial scenarios
(~10-20 minutes).

- `test-integration.R`: Bundle validation, simulation, fitted/predict, PIT,
  heavy helper coverage, cluster workflow tests, and performance checks.

## Writing New Tests

### Using Skip Conditions

Always use appropriate skip conditions for MCMC-dependent tests:

```r
# For tests requiring MCMC (ci level)
test_that("my MCMC test", {
  skip_if_not_test_level("ci")

  # ... test code with MCMC
})

# For exhaustive/slow tests (full level only)
test_that("exhaustive grid test", {
  skip_if_not_full()

  # ... expensive test code
})

# Skip during coverage runs (for covr/nimble conflicts)
test_that("test with covr issues", {
  skip_if(
    nzchar(Sys.getenv("COVERAGE")),
    "Skipping during coverage due to covr/nimble interaction"
  )

  # ... test code
})

# Skip if optional package not installed
test_that("test needing ggplot2", {
  skip_if_not_installed("ggplot2")

  # ... test code
})
```

### Using MCMC Helper Functions

The helper files provide standardized MCMC settings:

```r
# Fast settings for quick tests (niter=20, nburnin=5)
mcmc_settings <- mcmc_fast(seed = 42L)

# Integration test settings (niter=50, nburnin=15)
mcmc_settings <- mcmc_integration(seed = 42L)

# Full test settings (niter=200, nburnin=50, thin=2)
mcmc_settings <- mcmc_full(seed = 42L)
```

### Using Representative Combos

For integration tests, use pre-defined representative combinations:

```r
test_that("workflow with representative combos", {
  skip_if_not_test_level("ci")

  combos <- representative_combos()  # For standard models
  # or
  combos <- representative_causal_combos()  # For causal models

  for (cfg in combos) {
    # Test with cfg$kernel, cfg$backend, cfg$GPD, etc.
  }
})
```

### Test Structure Template

```r
# test-myfeature.R
# Description of what this file tests
#
# Tier A (cran): Fast unit tests
# Tier B (ci):   Integration tests with MCMC
# Tier C (full): Exhaustive combinations

# =============================================================================
# Tier A (cran): Fast tests - no MCMC
# =============================================================================

test_that("basic input validation works", {
  # Fast tests that don't require MCMC
  expect_error(my_function(NULL), "cannot be NULL")
})

# =============================================================================
# Tier B (ci): Integration tests
# =============================================================================

test_that("full workflow works", {
  skip_if_not_test_level("ci")

  mcmc <- mcmc_fast(seed = 1L)
  # ... MCMC-dependent tests
})

# =============================================================================
# Tier C (full): Exhaustive tests
# =============================================================================

test_that("all kernel combinations work", {
  skip_if_not_full()

  for (kernel in names(get_kernel_registry())) {
    # ... test each kernel
  }
})
```

## Coverage Reports

Coverage is managed through a single local script at `tools/.Rscripts/coverage.R`.
It runs the restored high-coverage test entrypoints, writes canonical artifacts
to `covr/assets/`, and mirrors the same report to `docs/coverage/`.

### Generate Local HTML Coverage Report

```r
source("tools/.Rscripts/coverage.R")
coverage_report()
```

This creates:

- `covr/assets/index.html` - Canonical tracked summary page
- `covr/assets/report.html` - Canonical tracked interactive covr report
- `covr/assets/cobertura.xml` - Canonical tracked Cobertura XML for Codecov uploads
- `covr/assets/coverage_status.json` - Canonical tracked JSON summary
- `covr/assets/unused_functions.md` - Canonical tracked unused-function report
- `docs/coverage/index.html` - Summary page with badge and file table
- `docs/coverage/report.html` - Full interactive covr report
- `docs/coverage/cobertura.xml` - Mirrored Cobertura XML
- `docs/coverage/coverage_status.json` - JSON data for CI
- `docs/coverage/unused_functions.md` - Published unused-function report
- `lib/` support directories in both retained locations so `report.html` stays functional
- When run via `Rscript tools/.Rscripts/coverage.R` (or `tools/coverage.bat`), it
  generates the same canonical local coverage report

### GitHub Actions Upload-Only Workflow

Coverage is still generated locally, not in GitHub Actions. On each push, the
workflow at `.github/workflows/codecov-upload.yml` uploads the committed
`covr/assets/cobertura.xml` report to Codecov.

If the Cobertura XML file is missing, empty, invalid, or the `CODECOV_TOKEN`
secret is not configured, the workflow exits successfully without uploading.

### Calculate Coverage Only

```r
source("tools/.Rscripts/coverage.R")

# Get the canonical coverage object without generating the HTML report
cov <- calculate_coverage()

# Use with covr functions
covr::percent_coverage(cov)
covr::report(cov)
```

### Coverage During Tests

During coverage calculation:

- `COVERAGE=1` environment variable is set
- `DPMIXGPD_TEST_LEVEL` is set from `DPMIXGPD_COVERAGE_LEVEL` (default: `"ci"`)
- `DPMIXGPD_CI_COVERAGE_ONLY=1` enables the dedicated coverage-only test file
  selected by the coverage runner

## Environment Variables Reference

| Variable                     | Values         | Default   | Purpose                            |
|-----------------------------|----------------|-----------|------------------------------------|
| `DPMIXGPD_TEST_LEVEL`       | cran, ci, full | cran      | Controls which tests run           |
| `DPMIXGPD_USE_CACHE`        | 0, 1           | 1         | Enable MCMC result caching         |
| `DPMIXGPD_CACHE_DIR`        | path           | _cache/   | Cache directory location           |
| `DPMIXGPD_TEST_VERBOSE`     | 0, 1           | 0         | Verbose test output                |
| `COVERAGE`                  | any non-empty  | empty     | Set during coverage runs           |
| `DPMIXGPD_CI_COVERAGE_ONLY` | 0, 1           | 0         | Enables the dedicated coverage-only test file |

## Troubleshooting

### Tests Are Too Slow

- Ensure you're running at "cran" level for development
- Enable caching: `Sys.setenv(DPMIXGPD_USE_CACHE = "1")`
- Run specific test files instead of the full suite

### NIMBLE Compilation Errors

- Clear the nimble cache: `nimble::clearCompiled()`
- Restart R session
- Check that Rtools (Windows) or Xcode CLI (macOS) is installed

### Coverage Calculation Fails

- The error "variable name conflicts with C++ keywords" is a known covr/nimble interaction
- Such tests should be skipped with `skip_if(nzchar(Sys.getenv("COVERAGE")))`

### Tests Pass Locally but Fail in CI

- Check the test level: CI may run at "ci" level vs local "cran"
- Ensure random seeds are set for reproducibility
- Check for platform-specific issues (Windows vs Unix paths, etc.)

## See Also

- [COVERAGE_MAP.md](COVERAGE_MAP.md) - Mapping of tests to source files
- [CONTRIBUTING.md](../../Misc/CONTRIBUTING.md) - General contribution guidelines
