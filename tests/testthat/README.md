# CausalMixGPD Test Suite Documentation

This document describes the testing infrastructure for the CausalMixGPD package.

## Test Tier System

The package uses a three-tier testing system to balance thoroughness with speed. Tests are controlled by the `DPMIXGPD_TEST_LEVEL` environment variable.

| Level  | Description                          | When to Use              | Approx. Time  |
|--------|--------------------------------------|--------------------------|---------------|
| `cran` | Fast unit tests, no MCMC compilation | Default, quick feedback  | ~1-2 minutes  |
| `ci`   | Integration tests with MCMC          | CI pipelines, PR checks  | ~10-20 minutes|
| `full` | Exhaustive kernel/backend grid       | Pre-release validation   | ~1+ hours     |

### Why Tiers?

MCMC tests require NIMBLE C++ compilation, which is computationally expensive. Running all tests during every development cycle would be impractical. The tiered system allows:

- **Fast feedback** during development (cran level)
- **Thorough testing** in CI pipelines (ci level)
- **Exhaustive testing** before releases (full level)

## Running Tests

### Basic Commands

```r
# Run tests at default (cran) level - fast unit tests only
testthat::test_local()

# Or equivalently
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
# Run a single test file
testthat::test_file("tests/testthat/test-causal.R")

# Run tests matching a pattern
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

### Helper Files

| File                          | Purpose                                    |
|-------------------------------|--------------------------------------------|
| `setup.R`                     | Global test setup, runs before all tests   |
| `helper-test-levels.R`        | Tier system functions and skip helpers     |
| `helper-cache.R`              | MCMC result caching infrastructure         |
| `helper-predict-distribution.R` | Prediction test utilities                |

### Test Files by Category

#### Core Functionality
| File                     | What it Tests                              |
|--------------------------|--------------------------------------------|
| `test-bundle.R`          | Bundle creation and validation             |
| `test-fitted.R`          | Fitted values and residuals                |
| `test-global-contracts.R`| Internal validation functions              |
| `test-kernel-registry.R` | Kernel registry functions                  |
| `test-kernel-support.R`  | Kernel support table                       |
| `test-kernels.R`         | NIMBLE kernel compilation                  |

#### Kernel-Specific Tests
| File                | Kernel Tested    |
|---------------------|------------------|
| `test-normal.R`     | Normal kernel    |
| `test-gamma.R`      | Gamma kernel     |
| `test-lognormal.R`  | Lognormal kernel |
| `test-invgauss.R`   | Inverse Gaussian |
| `test-laplace.R`    | Laplace kernel   |
| `test-amoroso.R`    | Amoroso kernel   |
| `test-cauchy.R`     | Cauchy kernel    |

#### Prediction Tests
| File                          | What it Tests                     |
|-------------------------------|-----------------------------------|
| `test-predict-unconditional.R`| Unconditional predictions         |
| `test-predict-contracts.R`    | Prediction interface contracts    |

#### Causal Inference Tests
| File                        | What it Tests                       |
|-----------------------------|-------------------------------------|
| `test-causal.R`             | Causal workflow, PS models          |
| `test-causal-ate.R`         | Average treatment effects           |
| `test-causal-predict.R`     | Causal predictions                  |
| `test-s3-causal-effects.R`  | S3 methods for causal objects       |
| `test-plots-causal-effects.R`| Causal effect plotting             |

#### Other Tests
| File                          | What it Tests                      |
|-------------------------------|------------------------------------|
| `test-theory.R`               | Statistical theory validation      |
| `test-visualization-helpers.R`| Plotting helper functions          |
| `test-simulated-data.R`       | Data simulation functions          |
| `test-status.R`               | Coverage status functions          |
| `test-smoke-core-workflows.R` | End-to-end smoke tests             |
| `test-hpd-intervals.R`        | HPD interval calculations          |
| `test-vectorization.R`        | Vectorized operations              |
| `test-dispatch-separation.R`  | Method dispatch logic              |

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
  skip_if(nzchar(Sys.getenv("COVERAGE")), 
          "Skipping during coverage due to covr/nimble interaction")
  
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

Coverage is managed through a unified script at `tools/.Rscripts/coverage.R` that supports
multiple coverage sources and output formats.

### Coverage Sources

| Source | Description | Speed |
|--------|-------------|-------|
| `tests` | testthat tests from `tests/testthat/` | Medium |
| `examples` | `@examples` from roxygen documentation | Fast |
| `vignettes` | Code chunks from R Markdown vignettes | Slow |
| `all` | All three sources combined | Slowest |

**Default:** `"tests"` at `DPMIXGPD_TEST_LEVEL = "ci"` - CI-level test coverage.

### Generate Local HTML Coverage Report

```r
source("tools/.Rscripts/coverage.R")

# Default: CI-level tests
coverage_report()

# Tests only (fastest)
coverage_report(sources = "tests")

# All sources (most comprehensive)
coverage_report(sources = "all")

# Custom output directory
coverage_report(output_dir = "my_coverage")
```

This creates:
- `docs/coverage/index.html` - Summary page with badge and file table
- `docs/coverage/report.html` - Full interactive covr report
- `docs/coverage/coverage_status.json` - JSON data for CI
- Copies in `pkgdown/assets/coverage/` for site integration
- When run via `Rscript tools/.Rscripts/coverage.R` (or `tools/coverage.bat`), it
  generates local coverage artifacts only (no automatic Codecov upload).
- To upload to Codecov manually, call `coverage_upload()`.

### Upload to Codecov

```r
source("tools/.Rscripts/coverage.R")

# Upload with default sources (requires CODECOV_TOKEN)
coverage_upload()

# Upload with specific sources
coverage_upload(sources = "all")
```

### Calculate Coverage Only

```r
source("tools/.Rscripts/coverage.R")

# Get coverage object without generating report
cov <- calculate_coverage()
cov <- calculate_coverage(sources = "tests", test_level = "full")

# Use with covr functions
covr::percent_coverage(cov)
covr::report(cov)
```

### Coverage During Tests

During coverage calculation:
- `COVERAGE=1` environment variable is set
- `DPMIXGPD_TEST_LEVEL` is set to "ci" by default
- Some tests are automatically skipped to avoid covr/nimble conflicts

## Environment Variables Reference

| Variable                  | Values              | Default  | Purpose                          |
|---------------------------|---------------------|----------|----------------------------------|
| `DPMIXGPD_TEST_LEVEL`     | cran, ci, full      | cran     | Controls which tests run         |
| `DPMIXGPD_USE_CACHE`      | 0, 1                | 1        | Enable MCMC result caching       |
| `DPMIXGPD_CACHE_DIR`      | path                | _cache/  | Cache directory location         |
| `DPMIXGPD_TEST_VERBOSE`   | 0, 1                | 0        | Verbose test output              |
| `COVERAGE`                | (any non-empty)     | (empty)  | Set during coverage runs         |

## Troubleshooting

### Tests are too slow

- Ensure you're running at "cran" level for development
- Enable caching: `Sys.setenv(DPMIXGPD_USE_CACHE = "1")`
- Run specific test files instead of the full suite

### NIMBLE compilation errors

- Clear the nimble cache: `nimble::clearCompiled()`
- Restart R session
- Check that Rtools (Windows) or Xcode CLI (macOS) is installed

### Coverage calculation fails

- The error "variable name conflicts with C++ keywords" is a known covr/nimble interaction
- Such tests should be skipped with `skip_if(nzchar(Sys.getenv("COVERAGE")))`

### Tests pass locally but fail in CI

- Check the test level: CI may run at "ci" level vs local "cran"
- Ensure random seeds are set for reproducibility
- Check for platform-specific issues (Windows vs Unix paths, etc.)

## See Also

- [COVERAGE_MAP.md](COVERAGE_MAP.md) - Mapping of tests to source files
- [CONTRIBUTING.md](../../Misc/CONTRIBUTING.md) - General contribution guidelines

