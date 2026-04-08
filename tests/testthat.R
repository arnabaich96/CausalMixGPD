library(testthat)
testthat::set_max_fails(Inf)

# Keep package checks at CRAN-level tests, regardless of ambient CI vars.
is_pkg_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
  nzchar(Sys.getenv("RCMDCHECK")) ||
  identical(tolower(Sys.getenv("NOT_CRAN", "")), "false")

if (is_pkg_check) {
  Sys.setenv(DPMIXGPD_TEST_LEVEL = "cran")
}

# During explicit coverage runs, default to CI-level tests unless overridden.
if (!is_pkg_check &&
    (nzchar(Sys.getenv("DPMIXGPD_COVERAGE")) ||
     nzchar(Sys.getenv("COVERAGE")) ||
     nzchar(Sys.getenv("R_COVR")) ||
     any(grepl("covr", loadedNamespaces(), ignore.case = TRUE))) &&
    !nzchar(Sys.getenv("DPMIXGPD_TEST_LEVEL"))) {
  Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci")
}

test_check("CausalMixGPD")
