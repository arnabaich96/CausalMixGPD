source("tools/.Rscripts/coverage.R")
Sys.setenv(DPMIXGPD_COVERAGE_LEVEL = "ci")
Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci", COVERAGE = "1", DPMIXGPD_CI_COVERAGE_ONLY = "1")
.check_coverage_deps()
.set_covr_install_opts()
res <- tryCatch({
  covr::package_coverage(
    type = "none",
    code = .coverage_test_code(progress = FALSE),
    quiet = FALSE,
    pre_clean = TRUE,
    INSTALL_opts = .coverage_install_opts(),
    line_exclusions = .coverage_line_exclusions()
  )
}, error = function(e) {
  cat("CUSTOM_MODE_ERROR\n")
  cat(conditionMessage(e), "\n", sep = "")
  quit(status = 1)
})
cat("CUSTOM_MODE_OK\n")
cat(covr::percent_coverage(res), "\n")
