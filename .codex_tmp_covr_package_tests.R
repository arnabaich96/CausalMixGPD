options(error = function() { traceback(2); quit(status = 1) })
source("tools/.Rscripts/coverage.R")
Sys.setenv(DPMIXGPD_COVERAGE_LEVEL = "ci")
Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci", COVERAGE = "1")
Sys.unsetenv("DPMIXGPD_CI_COVERAGE_ONLY")
.check_coverage_deps()
.set_covr_install_opts()
res <- covr::package_coverage(
  type = "tests",
  quiet = FALSE,
  pre_clean = TRUE,
  INSTALL_opts = .coverage_install_opts(),
  line_exclusions = .coverage_line_exclusions()
)
cat("PACKAGE_TESTS_OK\n")
cat(covr::percent_coverage(res), "\n")
