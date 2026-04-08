set_max_fails(Inf)
options(testthat.reporter = "summary")

is_pkg_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
  nzchar(Sys.getenv("RCMDCHECK")) ||
  identical(tolower(Sys.getenv("NOT_CRAN", "")), "false")

cache_default <- if (is_pkg_check) "0" else "1"
Sys.setenv(DPMIXGPD_USE_CACHE = Sys.getenv("DPMIXGPD_USE_CACHE", cache_default))

is_covr_run <- isTRUE(getOption("covr", FALSE)) ||
  nzchar(Sys.getenv("DPMIXGPD_COVERAGE")) ||
  nzchar(Sys.getenv("COVERAGE")) ||
  nzchar(Sys.getenv("R_COVR"))

if (is_pkg_check && !nzchar(Sys.getenv("DPMIXGPD_TEST_LEVEL"))) {
  Sys.setenv(DPMIXGPD_TEST_LEVEL = "cran")
}

if (!is_pkg_check && is_covr_run &&
  !nzchar(Sys.getenv("DPMIXGPD_TEST_LEVEL"))) {
  Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci")
}

if (exists("init_kernel_registry", mode = "function", inherits = TRUE)) {
  try(init_kernel_registry(), silent = TRUE)
}
