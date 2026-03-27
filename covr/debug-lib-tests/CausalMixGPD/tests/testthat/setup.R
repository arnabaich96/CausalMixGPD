set_max_fails(Inf)
options(testthat.reporter = "summary")
Sys.setenv(DPMIXGPD_USE_CACHE = Sys.getenv("DPMIXGPD_USE_CACHE", "1"))

is_pkg_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
  nzchar(Sys.getenv("RCMDCHECK")) ||
  identical(tolower(Sys.getenv("NOT_CRAN", "")), "false")

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

if (!is_covr_run) {
  pkg_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
  r_dir <- file.path(pkg_root, "R")
  r_files <- list.files(r_dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
  for (f in sort(r_files)) {
    tryCatch(
      {
        txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
        if (length(txt) > 0L) {
          txt[1] <- sub("^\\ufeff", "", txt[1])
        }
        eval(parse(text = txt, keep.source = FALSE), envir = .GlobalEnv)
      },
      error = function(e) {
        message("Skipping source file '", f, "': ", conditionMessage(e))
      }
    )
  }
}
if (exists("init_kernel_registry", mode = "function", inherits = TRUE)) {
  try(init_kernel_registry(), silent = TRUE)
}
