library(testthat)
library(CausalMixGPD)
library(crayon)
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


# Robustly locate the testthat directory relative to this script
test_dir_path <- NULL
pkg_root <- here::here()
this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
  if (basename(pkg_root) == "tests") {
library(testthat)
library(CausalMixGPD)
test_check("CausalMixGPD")
    test_dir_path <- file.path(pkg_root, "testthat")
}
if (is.null(test_dir_path) || !dir.exists(test_dir_path)) {
  test_dir_path <- file.path(pkg_root, "tests", "testthat")
}
