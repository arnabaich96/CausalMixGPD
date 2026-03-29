options(error = function() { traceback(2); quit(status = 1) })
source("tools/.Rscripts/coverage.R")
Sys.setenv(DPMIXGPD_COVERAGE_LEVEL = "ci")
Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci", COVERAGE = "1", DPMIXGPD_CI_COVERAGE_ONLY = "1")
.check_coverage_deps()
.set_covr_install_opts()
test_root <- file.path(".", "tests", "testthat")
tf <- c(
  file.path(test_root, "setup.R"),
  file.path(test_root, "helper-00-levels.R"),
  file.path(test_root, "helper-01-fixtures.R"),
  file.path(test_root, "helper-02-cache.R"),
  file.path(test_root, "helper-03-predict-helpers.R"),
  file.path(test_root, "test-ci-level-only.R"),
  file.path(test_root, "test-integration.R")
)
cat("TEST_FILES\n")
writeLines(basename(tf))
res <- covr::file_coverage(
  source_files = .coverage_source_files(),
  test_files = tf,
  line_exclusions = .coverage_line_exclusions()
)
cat("FILE_MODE_OK\n")
cat(covr::percent_coverage(res), "\n")
