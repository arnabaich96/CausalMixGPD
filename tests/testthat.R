library(testthat)
library(nimble)
library(DPmixGPD)
library(crayon)
testthat::set_max_fails(Inf)

# During covr/Codecov runs, default to CI-level tests unless explicitly overridden.
# This ensures integration tests (including NIMBLE workflows) contribute to coverage.
if ((nzchar(Sys.getenv("CODECOV_TOKEN")) ||
     nzchar(Sys.getenv("COVERAGE")) ||
     any(grepl("covr", loadedNamespaces(), ignore.case = TRUE))) &&
    !nzchar(Sys.getenv("DPMIXGPD_TEST_LEVEL"))) {
  Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci")
}

test_check("DPmixGPD")
