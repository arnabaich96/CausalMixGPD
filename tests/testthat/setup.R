set_max_fails(Inf)
options(testthat.reporter = "summary")
Sys.setenv(DPMIXGPD_USE_CACHE = Sys.getenv("DPMIXGPD_USE_CACHE", "1"))

# Skip problematic tests during codecov runs
# Detect if running under codecov by checking for CODECOV_TOKEN or covr environment
if (nzchar(Sys.getenv("CODECOV_TOKEN")) ||
    nzchar(Sys.getenv("COVERAGE")) ||
    any(grepl("covr", loadedNamespaces(), ignore.case = TRUE))) {
  # Set test level to "cran" to skip "ci" level tests that may fail during codecov
  if (!nzchar(Sys.getenv("DPMIXGPD_TEST_LEVEL"))) {
    Sys.setenv(DPMIXGPD_TEST_LEVEL = "cran")
  }
}
