options(error = function() { traceback(2); quit(status = 1) })
source("tools/.Rscripts/coverage.R")
Sys.setenv(DPMIXGPD_COVERAGE_LEVEL = "ci")
Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci", COVERAGE = "1", DPMIXGPD_CI_COVERAGE_ONLY = "1")
.check_coverage_deps()
.set_covr_install_opts()
tf <- .coverage_test_files()
cat("TEST_FILES\n")
writeLines(basename(tf))
res <- covr::file_coverage(
  source_files = .coverage_source_files(),
  test_files = tf,
  line_exclusions = .coverage_line_exclusions()
)
cat("FILE_MODE_OK\n")
cat(covr::percent_coverage(res), "\n")
