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

# testthat 3.x doesn't support recursive test discovery by default
# So we manually discover test files in subdirectories
test_dir_path <- if (basename(getwd()) == "tests") {
  "testthat"
} else if (file.exists("tests/testthat")) {
  "tests/testthat"
} else {
  stop("Cannot locate tests/testthat directory")
}

# Load all helper files first (testthat convention)
helper_files <- list.files(test_dir_path, pattern = "^helper.*\\.R$", full.names = TRUE)
for (helper in helper_files) {
  tryCatch(
    source(helper, local = .GlobalEnv),
    error = function(e) message("Helper load failed: ", basename(helper), " - ", e$message)
  )
}

# Load setup.R if present
setup_file <- file.path(test_dir_path, "setup.R")
if (file.exists(setup_file)) {
  tryCatch(
    source(setup_file, local = .GlobalEnv),
    error = function(e) message("Setup failed: ", e$message)
  )
}

# Discover test files recursively
test_files <- list.files(
  test_dir_path,
  pattern = "^test.*\\.R$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(test_files) == 0) {
  stop("No test files found in ", test_dir_path, " or subdirectories")
}

# Run tests for each file
reporter <- testthat::get_reporter()
for (test_file in test_files) {
  testthat::test_file(test_file, reporter = reporter, package = "CausalMixGPD", env = .GlobalEnv)
}
