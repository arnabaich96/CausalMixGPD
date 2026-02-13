set_max_fails(Inf)
options(testthat.reporter = "summary")
Sys.setenv(DPMIXGPD_USE_CACHE = Sys.getenv("DPMIXGPD_USE_CACHE", "1"))

is_pkg_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
  nzchar(Sys.getenv("RCMDCHECK")) ||
  identical(tolower(Sys.getenv("NOT_CRAN", "")), "false")

if (is_pkg_check && !nzchar(Sys.getenv("DPMIXGPD_TEST_LEVEL"))) {
  Sys.setenv(DPMIXGPD_TEST_LEVEL = "cran")
}

if (!is_pkg_check &&
    (isTRUE(getOption("covr", FALSE)) ||
     nzchar(Sys.getenv("DPMIXGPD_COVERAGE")) ||
     nzchar(Sys.getenv("COVERAGE")) ||
     nzchar(Sys.getenv("R_COVR"))) &&
    !nzchar(Sys.getenv("DPMIXGPD_TEST_LEVEL"))) {
  Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci")
}
