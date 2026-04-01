# Dedicated maximum-coverage entrypoint for covr runs.
#
# This suite is coverage-only. It reuses the targeted integration fragment
# that already scales down data sizes and MCMC settings when COVERAGE=1.

if (!identical(Sys.getenv("DPMIXGPD_CI_COVERAGE_ONLY"), "1")) {
  testthat::skip("Coverage-only suite disabled. Set DPMIXGPD_CI_COVERAGE_ONLY=1 to run.")
}

skip_if_not_test_level("ci")

source(
  testthat::test_path("fragments", "integration", "coverage_heavy.R"),
  local = TRUE,
  encoding = "UTF-8"
)
