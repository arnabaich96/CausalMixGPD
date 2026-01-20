if (!nzchar(Sys.getenv("CODECOV_TOKEN"))) {
  stop("CODECOV_TOKEN is not set. Restart RStudio or set it via setx.")
}

# Set test level to "cran" to skip problematic tests during codecov runs
Sys.setenv(DPMIXGPD_TEST_LEVEL = "cran")

devtools::load_all()
covr::codecov()

