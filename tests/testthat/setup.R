set_max_fails(Inf)
options(testthat.reporter = "summary")
Sys.setenv(DPMIXGPD_USE_CACHE = Sys.getenv("DPMIXGPD_USE_CACHE", "1"))

# During covr/Codecov runs, default to CI-level tests unless explicitly overridden.
# This ensures integration tests (including NIMBLE workflows) contribute to coverage.
if ((nzchar(Sys.getenv("CODECOV_TOKEN")) ||
     nzchar(Sys.getenv("COVERAGE")) ||
     any(grepl("covr", loadedNamespaces(), ignore.case = TRUE))) &&
    !nzchar(Sys.getenv("DPMIXGPD_TEST_LEVEL"))) {
  Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci")
}
