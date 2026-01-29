if (!requireNamespace("covr", quietly = TRUE)) {
  install.packages("covr", repos = "https://cloud.r-project.org")
}

source("tools/coverage.R")
.set_covr_install_opts()

Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci")
Sys.setenv(COVERAGE = "1")

cov <- covr::package_coverage(
  type = "none",
  code = "testthat::test_dir('tests/testthat', stop_on_failure = FALSE, reporter = 'progress')",
  quiet = FALSE,
  pre_clean = TRUE,
  line_exclusions = .coverage_line_exclusions()
)

cat("PERCENT=", covr::percent_coverage(cov), "\n")

