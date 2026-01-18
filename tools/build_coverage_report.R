if (!nzchar(Sys.getenv("CODECOV_TOKEN"))) {
  stop("CODECOV_TOKEN is not set. Restart RStudio or set it via setx.")
}

devtools::load_all()
covr::codecov()

