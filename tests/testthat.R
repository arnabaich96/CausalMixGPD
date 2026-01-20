library(testthat)
library(nimble)
library(DPmixGPD)
library(crayon)
testthat::set_max_fails(Inf)

# Set test level to "cran" for codecov runs to skip problematic tests
# This ensures tests that may fail during codecov are skipped
if (nzchar(Sys.getenv("CODECOV_TOKEN")) || 
    nzchar(Sys.getenv("COVERAGE")) ||
    any(grepl("covr", loadedNamespaces(), ignore.case = TRUE))) {
  if (!nzchar(Sys.getenv("DPMIXGPD_TEST_LEVEL"))) {
    Sys.setenv(DPMIXGPD_TEST_LEVEL = "cran")
  }
}

test_check("DPmixGPD")
