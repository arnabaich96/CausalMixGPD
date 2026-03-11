# ============================================================================
# Unified Coverage Calculation and Reporting for CausalMixGPD
# ============================================================================
#
# This script provides functions for calculating package coverage and generating
# reports. It supports multiple coverage sources: tests, examples, and vignettes.
#
# Usage:
#   source("tools/.Rscripts/coverage.R")
#
#   # Generate local HTML report (default: CI-level tests)
#   coverage_report()
#
#   # Upload to Codecov
#   coverage_upload()
#
#   # Custom sources
#   coverage_report(sources = "all")
#   coverage_report(sources = "tests")
#   coverage_report(sources = c("tests", "examples", "vignettes"))
#
# ============================================================================
setwd(here::here())

# Check required packages
.check_coverage_deps <- function() {

  if (!requireNamespace("covr", quietly = TRUE)) {
    stop("Package 'covr' is required. Install with: install.packages('covr')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install with: install.packages('jsonlite')")
  }
  invisible(TRUE)
}

# Resolve coverage settings from environment overrides
.resolve_coverage_inputs <- function(sources, test_level) {
  env_sources <- Sys.getenv("DPMIXGPD_COVERAGE_SOURCES", "")
  if (nzchar(env_sources)) {
    sources <- strsplit(env_sources, "\\s*,\\s*")[[1]]
  }
  env_level <- Sys.getenv("DPMIXGPD_TEST_LEVEL", "")
  if (nzchar(env_level)) {
    test_level <- tolower(env_level)
  }
  list(sources = sources, test_level = test_level)
}

# Avoid slow vignette/manual builds during coverage installs
.set_covr_install_opts <- function() {
  options(covr.install = c("--no-build-vignettes", "--no-manual"))
  invisible(TRUE)
}

.coverage_excluded_files <- function() {
  c("1-registry.R", "01-registry.R", "zzz.R")
}

.coverage_current_r_files <- function(path = ".") {
  files <- list.files(file.path(path, "R"), pattern = "\\.[Rr]$", full.names = FALSE)
  unique(c(files, .coverage_excluded_files()))
}

.coverage_clean_dir <- function(path) {
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.coverage_refresh_metadata <- function(path = ".") {
  if (!file.exists(file.path(path, "DESCRIPTION"))) return(invisible(FALSE))
  tryCatch({
    pkgload::load_all(path = path, export_all = FALSE, helpers = FALSE, quiet = TRUE)
    if (isNamespaceLoaded("CausalMixGPD")) unloadNamespace("CausalMixGPD")
    TRUE
  }, error = function(e) FALSE)
}

.filter_coverage_object <- function(cov, path = ".") {
  if (is.null(cov) || !inherits(cov, "coverage")) return(cov)

  nm <- names(cov)
  if (is.null(nm) || !length(nm)) return(cov)

  keep_files <- .coverage_current_r_files(path = path)
  keep <- sub(":.*$", "", nm) %in% keep_files
  cov[keep]
}

.coverage_test_files <- function() {
  env_files <- Sys.getenv("DPMIXGPD_COVERAGE_TEST_FILES", "")
  if (nzchar(env_files)) {
    return(trimws(strsplit(env_files, "\\s*,\\s*")[[1]]))
  }

  # Exclude test_cluster_fit_predict.R by default: it passes in testthat but
  # currently corrupts covr trace merging ("error reading from connection").
  c(
    "test-progress.R",
    "test-unit.R",
    "test-coverage-heavy.R",
    "test-integration.R",
    "test-ci.R",
    "test_cluster_methods.R",
    "test-performance-acceptance.R",
    "test-performance-phase2-ess.R",
    "test-performance-phase2-predict.R",
    "test-performance-phase2-samplers.R",
    "test-performance-phase2-zupdate.R",
    "test-ci-level-only.R"
  )
}

.coverage_test_code_selected_files <- function(test_files = .coverage_test_files(), progress = FALSE) {
  reporter_expr <- if (isTRUE(progress)) {
    "testthat::ProgressReporter$new(show_praise = FALSE)"
  } else {
    "testthat::ProgressReporter$new(show_praise = FALSE, update_interval = Inf)"
  }
  test_files_expr <- paste(sprintf('"%s"', test_files), collapse = ", ")

  paste(
    "library(CausalMixGPD)",
    'pkg_tests <- system.file("tests", "testthat", package = "CausalMixGPD")',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) {',
    '  local_candidates <- c("tests/testthat", "./tests/testthat")',
    '  local_hit <- local_candidates[file.exists(local_candidates)]',
    '  if (length(local_hit) > 0L) pkg_tests <- normalizePath(local_hit[1], winslash = "/", mustWork = FALSE)',
    '}',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) stop("Coverage could not find test directory: tests/testthat")',
    sprintf("test_files <- c(%s)", test_files_expr),
    'targets <- file.path(pkg_tests, test_files)',
    'missing_targets <- targets[!file.exists(targets)]',
    'if (length(missing_targets) > 0L) stop("Coverage test file(s) not found: ", paste(missing_targets, collapse = ", "))',
    'Sys.setenv(COVERAGE = "1", DPMIXGPD_CI_COVERAGE_ONLY = "1")',
    'if (!nzchar(Sys.getenv("DPMIXGPD_SKIP_COVR_METHODS_BLOCK"))) Sys.setenv(DPMIXGPD_SKIP_COVR_METHODS_BLOCK = "1")',
    'if (!nzchar(Sys.getenv("DPMIXGPD_SKIP_COVR_CLUSTER_HELPERS"))) Sys.setenv(DPMIXGPD_SKIP_COVR_CLUSTER_HELPERS = "1")',
    'helper_files <- list.files(pkg_tests, pattern = "^helper.*\\\\.R$", full.names = TRUE)',
    'for (helper in helper_files) try(source(helper, local = .GlobalEnv), silent = TRUE)',
    'setup_file <- file.path(pkg_tests, "setup.R")',
    'if (file.exists(setup_file)) try(source(setup_file, local = .GlobalEnv), silent = TRUE)',
    sprintf('reporter <- %s', reporter_expr),
    'for (target in targets) testthat::test_file(target, reporter = reporter, package = "CausalMixGPD")',
    sep = "\n"
  )
}

# Test snippets for covr::package_coverage(type = "none").
# Attach package explicitly so batch runs (e.g., via .bat) execute tests with
# the same symbol resolution as interactive sessions.
# Updated to support recursive test discovery in subdirectories.
.coverage_test_code_minimal <- function() {
  paste(
    "library(CausalMixGPD)",
    'ns <- asNamespace("CausalMixGPD")',
    'for (nm in ls(ns, all.names = TRUE)) {',
    '  if (!exists(nm, envir = .GlobalEnv, inherits = FALSE)) {',
    '    tryCatch(assign(nm, get(nm, envir = ns, inherits = FALSE), envir = .GlobalEnv), error = function(e) NULL)',
    '  }',
    '}',
    'pkg_tests <- system.file("tests", "testthat", package = "CausalMixGPD")',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) {',
    '  local_candidates <- c("tests/testthat", "./tests/testthat")',
    '  local_hit <- local_candidates[file.exists(local_candidates)]',
    '  if (length(local_hit) > 0L) pkg_tests <- normalizePath(local_hit[1], winslash = "/", mustWork = FALSE)',
    '}',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) stop("Coverage could not find test directory: tests/testthat")',
    'if (nzchar(pkg_tests)) {',
    '  test_files <- list.files(pkg_tests, pattern = "^test.*\\\\.R$", recursive = TRUE, full.names = TRUE)',
    '  if (length(test_files) == 0L) stop("Coverage found no test files under: ", pkg_tests)',
    '  reporter <- testthat::ProgressReporter$new(show_praise = FALSE, update_interval = Inf)',
    '  testthat::test_dir(pkg_tests, reporter = reporter, package = "CausalMixGPD")',
    '}',
    sep = "\n"
  )
}

.coverage_test_code_progress <- function() {
  paste(
    "library(CausalMixGPD)",
    'ns <- asNamespace("CausalMixGPD")',
    'for (nm in ls(ns, all.names = TRUE)) {',
    '  if (!exists(nm, envir = .GlobalEnv, inherits = FALSE)) {',
    '    tryCatch(assign(nm, get(nm, envir = ns, inherits = FALSE), envir = .GlobalEnv), error = function(e) NULL)',
    '  }',
    '}',
    'pkg_tests <- system.file("tests", "testthat", package = "CausalMixGPD")',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) {',
    '  local_candidates <- c("tests/testthat", "./tests/testthat")',
    '  local_hit <- local_candidates[file.exists(local_candidates)]',
    '  if (length(local_hit) > 0L) pkg_tests <- normalizePath(local_hit[1], winslash = "/", mustWork = FALSE)',
    '}',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) stop("Coverage could not find test directory: tests/testthat")',
    'if (nzchar(pkg_tests)) {',
    '  test_files <- list.files(pkg_tests, pattern = "^test.*\\\\.R$", recursive = TRUE, full.names = TRUE)',
    '  if (length(test_files) == 0L) stop("Coverage found no test files under: ", pkg_tests)',
    '  reporter <- testthat::ProgressReporter$new()',
    '  testthat::test_dir(pkg_tests, reporter = reporter, package = "CausalMixGPD")',
    '}',
    sep = "\n"
  )
}

.coverage_test_code_fallback <- function() {
  paste(
    "library(CausalMixGPD)",
    'ns <- asNamespace("CausalMixGPD")',
    'for (nm in ls(ns, all.names = TRUE)) {',
    '  if (!exists(nm, envir = .GlobalEnv, inherits = FALSE)) {',
    '    tryCatch(assign(nm, get(nm, envir = ns, inherits = FALSE), envir = .GlobalEnv), error = function(e) NULL)',
    '  }',
    '}',
    'pkg_tests <- system.file("tests", "testthat", package = "CausalMixGPD")',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) {',
    '  local_candidates <- c("tests/testthat", "./tests/testthat")',
    '  local_hit <- local_candidates[file.exists(local_candidates)]',
    '  if (length(local_hit) > 0L) pkg_tests <- normalizePath(local_hit[1], winslash = "/", mustWork = FALSE)',
    '}',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) stop("Coverage could not find test directory: tests/testthat")',
    'if (nzchar(pkg_tests)) {',
    '  # Load helpers first',
    '  helper_files <- list.files(pkg_tests, pattern = "^helper.*\\\\.R$", full.names = TRUE)',
    '  for (helper in helper_files) try(source(helper, local = .GlobalEnv), silent = TRUE)',
    '  setup_file <- file.path(pkg_tests, "setup.R")',
    '  if (file.exists(setup_file)) try(source(setup_file, local = .GlobalEnv), silent = TRUE)',
    '  # Run tests',
    '  test_files <- list.files(pkg_tests, pattern = "^test.*\\\\.R$", recursive = TRUE, full.names = TRUE)',
    '  if (length(test_files) == 0L) stop("Coverage found no test files under: ", pkg_tests)',
    '  for (f in test_files) try(source(f), silent = TRUE)',
    '}',
    sep = "\n"
  )
}

# Files to exclude entirely from coverage
.coverage_line_exclusions <- function(path = ".") {
  files <- file.path("R", .coverage_excluded_files())
  full_paths <- normalizePath(file.path(path, files), winslash = "/", mustWork = FALSE)
  all_paths <- unique(c(files, full_paths))
  stats::setNames(as.list(rep(Inf, length(all_paths))), all_paths)
}

# Get Codecov token from environment.
.get_codecov_token <- function() {
  env_token <- Sys.getenv("CODECOV_TOKEN", "")
  if (nzchar(env_token)) return(env_token)

  return("")
}

# ============================================================================
# calculate_coverage: Core function to calculate package coverage
# ============================================================================

#' Calculate package coverage
#'
#' @param sources Character vector specifying coverage sources.
#'   Options: "tests", "examples", "vignettes", or "all".
#'   Default: "tests"
#' @param test_level Test tier for testthat: "cran", "ci", or "full".
#'   Default: "ci"
#' @param quiet Logical; suppress covr output? Default: FALSE
#' @return A covr coverage object, or NULL if coverage calculation failed
#'
#' @examples
#' \dontrun{
#' cov <- calculate_coverage()
#' cov <- calculate_coverage(sources = "tests")
#' cov <- calculate_coverage(sources = "all", test_level = "full")
#' }
calculate_coverage <- function(
    sources = "tests",
    test_level = "ci",
    quiet = FALSE
) {
  .check_coverage_deps()
  .set_covr_install_opts()

  # Validate inputs
  valid_sources <- c("tests", "examples", "vignettes", "all")
  if (!all(sources %in% valid_sources)) {
    stop("Invalid sources. Must be one or more of: ", paste(valid_sources, collapse = ", "))
  }

  # Expand "all" to individual sources

  if ("all" %in% sources) {
    sources <- c("tests", "examples", "vignettes")
  }

  # Set environment variables
  Sys.setenv(DPMIXGPD_TEST_LEVEL = test_level)
  Sys.setenv(COVERAGE = "1")

  # Unload package if loaded to avoid instrumentation conflicts

  if (isNamespaceLoaded("CausalMixGPD")) {
    if (!quiet) cat("Unloading CausalMixGPD namespace for clean coverage calculation...\n")
    try(unloadNamespace("CausalMixGPD"), silent = TRUE)
  }

  if (!quiet) {
    cat("Calculating coverage...\n")
    cat("  Sources:", paste(sources, collapse = ", "), "\n")
    cat("  Test level:", test_level, "\n\n")
  }

  # Build coverage based on sources
  cov <- tryCatch({
    if (identical(sources, "tests") || (length(sources) == 1 && sources == "tests")) {
      # Tests only - use robust custom execution
      .calculate_tests_coverage(quiet = quiet)
    } else if ("tests" %in% sources && length(sources) > 1) {
      # Tests + other sources - try combined approach
      .calculate_combined_coverage(sources, quiet = quiet)
    } else {
      # Non-test sources only (examples, vignettes)
      covr::package_coverage(
        type = setdiff(sources, "tests"),
        quiet = quiet,
        pre_clean = TRUE,
        line_exclusions = .coverage_line_exclusions()
      )
    }
  }, error = function(e) {
    if (!quiet) {
      cat("\nWarning: Primary coverage method encountered an error.\n")
      cat("Error:", conditionMessage(e), "\n")
    }
    NULL
  })

  cov <- .filter_coverage_object(cov)

  if (!quiet && !is.null(cov)) {
    cat("\nCoverage calculation complete.\n")
    cat("Overall coverage:", round(covr::percent_coverage(cov), 1), "%\n\n")
  }

  cov
}

# Internal: Calculate coverage from tests with robust error handling
.calculate_tests_coverage <- function(quiet = FALSE) {
  covr::package_coverage(
    type = "none",
    code = .coverage_test_code_selected_files(progress = !quiet),
    quiet = quiet,
    pre_clean = TRUE,
    line_exclusions = .coverage_line_exclusions()
  )
}

# Internal: Calculate combined coverage from multiple sources
.calculate_combined_coverage <- function(sources, quiet = FALSE) {
  non_test_sources <- setdiff(sources, "tests")

  if (length(non_test_sources) > 0 && "tests" %in% sources) {
    cov_tests <- .calculate_tests_coverage(quiet = quiet)
    cov_other <- covr::package_coverage(
      type = non_test_sources,
      quiet = quiet,
      pre_clean = TRUE,
      line_exclusions = .coverage_line_exclusions()
    )
    getFromNamespace("merge_coverage.list", "covr")(list(cov_tests, cov_other))
  } else if (length(non_test_sources) > 0) {
    covr::package_coverage(
      type = non_test_sources,
      quiet = quiet,
      pre_clean = TRUE,
      line_exclusions = .coverage_line_exclusions()
    )
  } else {
    .calculate_tests_coverage(quiet = quiet)
  }
}

# Run test coverage with progress reporter (replacement for tools/run_covr_progress.R).
coverage_progress <- function(test_level = "ci", quiet = FALSE) {
  .check_coverage_deps()
  .set_covr_install_opts()

  Sys.setenv(DPMIXGPD_TEST_LEVEL = tolower(test_level))
  Sys.setenv(COVERAGE = "1")
  Sys.setenv(DPMIXGPD_CI_COVERAGE_ONLY = "1")
  Sys.setenv(DPMIXGPD_SKIP_COVR_METHODS_BLOCK = "1")

  cov <- covr::package_coverage(
    type = "none",
    code = .coverage_test_code_selected_files(progress = !quiet),
    quiet = quiet,
    pre_clean = TRUE,
    line_exclusions = .coverage_line_exclusions()
  )

  pct <- covr::percent_coverage(cov)
  cat("PERCENT=", pct, "\n")
  invisible(cov)
}

# ============================================================================
# coverage_report: Generate local HTML coverage report
# ============================================================================

#' Generate local HTML coverage report
#'
#' Calculates coverage and generates an interactive HTML report for the
#' pkgdown website.
#'
#' @param sources Character vector specifying coverage sources.
#'   Default: "tests"
#' @param test_level Test tier: "cran", "ci", or "full". Default: "ci"
#' @param output_dir Output directory. Default: "docs/coverage"
#' @param browse Logical; open report in browser? Default: FALSE
#' @return Invisibly returns the coverage object
#'
#' @examples
#' \dontrun{
#' # Default: CI-level tests
#' coverage_report()
#'
#' # Tests only (fastest)
#' coverage_report(sources = "tests")
#'
#' # All sources
#' coverage_report(sources = "all")
#'
#' # Custom output directory
#' coverage_report(output_dir = "my_coverage")
#' }
coverage_report <- function(
    sources = "tests",
    test_level = "ci",
  output_dir = "docs/coverage",
  browse = FALSE
) {
  .check_coverage_deps()
  resolved <- .resolve_coverage_inputs(sources, test_level)
  sources <- resolved$sources
  test_level <- resolved$test_level

  assets_dir <- "covr/assets/"

  .coverage_clean_dir(output_dir)
  .coverage_clean_dir(assets_dir)
  .coverage_refresh_metadata()

  cat("============================================================\n")
  cat("Building Coverage Report\n")
  cat("============================================================\n")
  cat("Sources:", paste(sources, collapse = ", "), "\n")
  cat("Test level:", test_level, "\n")
  cat("Output directory:", output_dir, "\n\n")

  cat("Prepared clean output directories.\n")

  # Step 1: Calculate coverage
  cat("\nStep 1/5: Calculating package coverage...\n")
  cov <- calculate_coverage(sources = sources, test_level = test_level, quiet = FALSE)

  if (is.null(cov)) {
    stop("Coverage calculation failed; report generation aborted.", call. = FALSE)
  }

  # Step 2: Generate interactive HTML report
  cat("Step 2/5: Generating interactive HTML report...\n")
  report_file <- file.path(output_dir, "report.html")
  covr::report(cov, file = report_file, browse = FALSE)
  cat("Saved:", report_file, "\n")

  assets_report <- file.path(assets_dir, "report.html")
  file.copy(report_file, assets_report, overwrite = TRUE)
  cat("Copied to:", assets_report, "\n\n")

  # Step 3: Extract and save statistics
  cat("Step 3/5: Extracting coverage statistics...\n")
  stats <- .extract_coverage_stats(cov, sources)
  json_file <- file.path(output_dir, "coverage_status.json")
  jsonlite::write_json(stats$summary, json_file, auto_unbox = TRUE, pretty = TRUE)
  cat("Saved:", json_file, "\n")

  assets_json <- file.path(assets_dir, "coverage_status.json")
  file.copy(json_file, assets_json, overwrite = TRUE)
  cat("Copied to:", assets_json, "\n\n")

  # Step 4: Write unused function report
  cat("Step 4/5: Writing unused function report...\n")
  unused_file <- file.path(output_dir, "unused_functions.md")
  unused_by_file <- .extract_unused_functions(cov)
  .write_unused_functions_report(unused_by_file, unused_file, sources, test_level)
  cat("Saved:", unused_file, "\n")

  assets_unused <- file.path(assets_dir, "unused_functions.md")
  file.copy(unused_file, assets_unused, overwrite = TRUE)
  cat("Copied to:", assets_unused, "\n\n")

  # Step 5: Generate summary HTML page
  cat("Step 5/5: Generating HTML summary page...\n")
  html_file <- file.path(output_dir, "index.html")
  .generate_summary_html(stats, html_file, sources)
  cat("Saved:", html_file, "\n")

  assets_html <- file.path(assets_dir, "index.html")
  file.copy(html_file, assets_html, overwrite = TRUE)
  cat("Copied to:", assets_html, "\n\n")

  # Done
  cat("============================================================\n")
  cat("Coverage report generated successfully!\n")
  cat("============================================================\n\n")
  cat("Output files:\n")
  cat("  - ", html_file, " (summary page)\n", sep = "")
  cat("  - ", report_file, " (interactive report)\n", sep = "")
  cat("  - ", json_file, " (JSON data)\n", sep = "")
  cat("  - ", unused_file, " (unused functions)\n", sep = "")
  cat("\nOverall coverage: ", round(stats$percent, 1), "%\n", sep = "")

  if (browse) {
    utils::browseURL(html_file)
  }

  invisible(cov)
}

# ============================================================================
# coverage_upload: Upload coverage to Codecov
# ============================================================================

#' Upload coverage to Codecov
#'
#' Calculates coverage and uploads to Codecov. If no token is available,
#' the upload is skipped gracefully with a message (unless `require_token = TRUE`).
#'
#' The token is loaded from (in order of priority):
#' 1. The `token` argument if provided
#' 2. The `CODECOV_TOKEN` environment variable
#'
#' @param sources Character vector specifying coverage sources.
#'   Default: "tests"
#' @param test_level Test tier: "cran", "ci", or "full". Default: "ci"
#' @param token Codecov token. Default: auto-detected from env var or token file
#' @param require_token Logical; if TRUE, throw an error when token is missing.
#'   If FALSE, skip upload gracefully. Default: FALSE
#' @param coverage Optional precomputed coverage object from `calculate_coverage()`.
#'   If provided, upload uses this object and skips recalculation.
#' @param quiet Logical; suppress output? Default: FALSE
#' @return TRUE on success, FALSE if upload was skipped or failed
#'
#' @examples
#' \dontrun{
#' # Upload with default sources (skips gracefully if no token)
#' coverage_upload()
#'
#' # Upload with all sources
#' coverage_upload(sources = "all")
#'
#' # Require token (fail if not set)
#' coverage_upload(require_token = TRUE)
#' }
coverage_upload <- function(
    sources = "tests",
    test_level = "ci",
    token = .get_codecov_token(),
    require_token = FALSE,
    coverage = NULL,
    quiet = FALSE
) {
  .check_coverage_deps()
  resolved <- .resolve_coverage_inputs(sources, test_level)
  sources <- resolved$sources
  test_level <- resolved$test_level

  # Check if token is available
  token_available <- nzchar(token)

  if (!token_available) {
    if (require_token) {
      stop("CODECOV_TOKEN is not set. Set it via Sys.setenv() or environment variable.")
    }
    if (!quiet) {
      cat("============================================================\n")
      cat("Codecov Upload Skipped\n")
      cat("============================================================\n")
      cat("CODECOV_TOKEN environment variable is not set.\n")
      cat("To upload coverage, set CODECOV_TOKEN and re-run.\n")
      cat("============================================================\n")
    }
    return(invisible(FALSE))
  }

  if (!quiet) {
    cat("============================================================\n")
    cat("Uploading Coverage to Codecov\n")
    cat("============================================================\n")
    cat("Sources:", paste(sources, collapse = ", "), "\n")
    cat("Test level:", test_level, "\n\n")
  }

  result <- tryCatch({
    if (is.null(coverage)) {
      if (!quiet) cat("Step 1: Calculating package coverage...\n")
      cov <- calculate_coverage(sources = sources, test_level = test_level, quiet = quiet)
    } else {
      if (!quiet) cat("Step 1: Using precomputed coverage object...\n")
      cov <- coverage
    }

    if (is.null(cov)) {
      if (!quiet) cat("Coverage calculation failed.\n")
      return(FALSE)
    }

    if (!quiet) cat("Step 2: Uploading to Codecov...\n")
    covr::codecov(coverage = cov, token = token, quiet = quiet)

    if (!quiet) cat("\nCoverage upload completed successfully!\n")
    TRUE
  }, error = function(e) {
    if (!quiet) {
      cat("Error uploading coverage:", conditionMessage(e), "\n")
    }
    FALSE
  })

  invisible(result)
}

# ============================================================================
# Internal helper functions
# ============================================================================

# Extract coverage statistics
.extract_coverage_stats <- function(cov, sources) {
  percent <- covr::percent_coverage(cov)

  # Per-file coverage
  file_cov <- covr::tally_coverage(cov, by = "line")

  file_stats <- aggregate(
    cbind(lines = 1, covered = value > 0) ~ filename,
    data = file_cov,
    FUN = sum
  )
  file_stats$percent <- round(100 * file_stats$covered / file_stats$lines, 1)
  file_stats <- file_stats[order(-file_stats$percent, file_stats$filename), ]
  names(file_stats) <- c("File", "Lines", "Covered", "Percent")

  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  summary_data <- list(
    percent = round(percent, 2),
    timestamp = timestamp,
    sources = sources,
    total_lines = sum(file_stats$Lines),
    covered_lines = sum(file_stats$Covered),
    files = nrow(file_stats),
    file_coverage = lapply(seq_len(nrow(file_stats)), function(i) {
      list(
        file = file_stats$File[i],
        lines = file_stats$Lines[i],
        covered = file_stats$Covered[i],
        percent = file_stats$Percent[i]
      )
    })
  )

  list(
    percent = percent,
    timestamp = timestamp,
    file_stats = file_stats,
    summary = summary_data
  )
}

# Extract functions with 0% coverage grouped by file
.extract_unused_functions <- function(cov) {
  line_cov <- covr::tally_coverage(cov, by = "line")
  if (!is.data.frame(line_cov) || nrow(line_cov) == 0L) return(list())

  file_col <- if ("filename" %in% names(line_cov)) "filename" else if ("file" %in% names(line_cov)) "file" else NULL
  fun_col <- intersect(names(line_cov), c("function", "fun", "func"))
  value_col <- if ("value" %in% names(line_cov)) "value" else if ("covered" %in% names(line_cov)) "covered" else NULL

  if (is.null(file_col) || length(fun_col) == 0L || is.null(value_col)) return(list())

  fun_col <- fun_col[1]
  df <- data.frame(
    file = line_cov[[file_col]],
    func = line_cov[[fun_col]],
    value = line_cov[[value_col]],
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$func) & nzchar(df$func), , drop = FALSE]
  if (nrow(df) == 0L) return(list())

  df$total_line <- !is.na(df$value)
  df$covered_line <- df$value > 0

  agg <- aggregate(
    cbind(total_line, covered_line) ~ file + func,
    data = df,
    FUN = sum,
    na.rm = TRUE
  )
  unused <- agg[agg$covered_line == 0 & agg$total_line > 0, , drop = FALSE]
  if (nrow(unused) == 0L) return(list())

  unused <- unused[order(unused$file, unused$func), , drop = FALSE]
  split(unused$func, unused$file)
}

# Write unused function report
.write_unused_functions_report <- function(unused_by_file, output_file, sources, test_level) {
  timestamp <- format(Sys.time(), "%B %d, %Y at %H:%M UTC", tz = "UTC")
  lines <- c(
    "# Unused Functions (0% covered)",
    "",
    paste("**Generated:**", timestamp),
    paste("**Sources:**", paste(sources, collapse = ", ")),
    paste("**Test level:**", test_level),
    ""
  )

  if (length(unused_by_file) == 0L) {
    lines <- c(lines, "All functions have non-zero coverage.")
  } else {
    for (file in names(unused_by_file)) {
      lines <- c(lines, paste0("## `", file, "`"), "")
      funcs <- unused_by_file[[file]]
      lines <- c(lines, paste0("- `", funcs, "`"), "")
    }
  }

  writeLines(lines, output_file)
}

# Create placeholder report when coverage fails
.create_placeholder_report <- function(output_dir, sources) {
  cat("Coverage calculation failed. Creating placeholder report.\n")

  placeholder_md <- c(
    "# Test Coverage Report",
    "",
    "**Status:** Coverage calculation failed",
    "",
    paste("**Sources attempted:**", paste(sources, collapse = ", ")),
    "",
    "Please run the coverage script locally to generate the full report.",
    "",
    "```r",
    "source('tools/.Rscripts/coverage.R')",
    "coverage_report()",
    "```"
  )
  writeLines(placeholder_md, file.path(output_dir, "index.md"))

  placeholder_json <- list(
    percent = NA,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    sources = sources,
    status = "failed"
  )
  jsonlite::write_json(placeholder_json, file.path(output_dir, "coverage_status.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  cat("Placeholder files created. Please resolve issues and re-run.\n")
}

# Generate summary HTML page
.generate_summary_html <- function(stats, html_file, sources) {
  percent <- stats$percent
  file_stats <- stats$file_stats
  timestamp <- stats$timestamp
  use_docs_assets <- grepl("(^|[/\\\\])docs([/\\\\]|$)", normalizePath(dirname(html_file), winslash = "/", mustWork = FALSE))

  # Badge color
  badge_color <- if (percent >= 80) {
    "green"
  } else if (percent >= 60) {
    "yellow"
  } else if (percent >= 40) {
    "orange"
  } else {
    "red"
  }

  # Format timestamp
  display_time <- format(
    as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    "%B %d, %Y at %H:%M UTC"
  )

  # Build file table rows
  file_rows <- vapply(seq_len(nrow(file_stats)), function(i) {
    display_file <- gsub("^R/", "", file_stats$File[i])
    pct <- file_stats$Percent[i]

    color_class <- if (pct >= 80) {
      "text-success"
    } else if (pct >= 60) {
      "text-warning"
    } else if (pct >= 40) {
      "text-warning"
    } else {
      "text-danger"
    }

    sprintf('      <tr>
        <td><code>%s</code></td>
        <td class="text-end">%d</td>
        <td class="text-end">%d</td>
        <td class="text-end %s"><strong>%.1f%%</strong></td>
      </tr>',
            display_file, file_stats$Lines[i], file_stats$Covered[i], color_class, pct
    )
  }, character(1))

  # Sources display
  sources_display <- paste(sources, collapse = ", ")

  if (!use_docs_assets) {
    html_content <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Coverage - CausalMixGPD</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; margin: 2rem; color: #111827; background: #f8fafc; }
    main { max-width: 1100px; margin: 0 auto; background: white; padding: 2rem; border-radius: 16px; box-shadow: 0 10px 25px rgba(15,23,42,0.08); }
    table { border-collapse: collapse; width: 100%%; }
    th, td { padding: 0.6rem 0.75rem; border-bottom: 1px solid #e5e7eb; text-align: left; }
    th.num, td.num { text-align: right; }
    code { background: #f3f4f6; padding: 0.1rem 0.35rem; border-radius: 4px; }
    .good { color: #166534; }
    .mid { color: #92400e; }
    .bad { color: #b91c1c; }
    .actions a { margin-right: 1rem; }
  </style>
</head>
<body>
  <main>
    <h1>Coverage Report</h1>
    <p><strong>Generated:</strong> %s</p>
    <p><strong>Sources:</strong> %s</p>
    <p><strong>Overall coverage:</strong> %.1f%%</p>
    <p class="actions">
      <a href="report.html">Interactive report</a>
      <a href="coverage_status.json"><code>coverage_status.json</code></a>
      <a href="unused_functions.md"><code>unused_functions.md</code></a>
    </p>
    <table>
      <thead>
        <tr>
          <th>File</th>
          <th class="num">Lines</th>
          <th class="num">Covered</th>
          <th class="num">Coverage</th>
        </tr>
      </thead>
      <tbody>
%s
      </tbody>
    </table>
  </main>
</body>
</html>',
      display_time, sources_display, percent,
      paste(vapply(seq_len(nrow(file_stats)), function(i) {
        pct <- file_stats$Percent[i]
        cls <- if (pct >= 80) "good" else if (pct >= 60) "mid" else "bad"
        sprintf(
          '        <tr><td><code>%s</code></td><td class="num">%d</td><td class="num">%d</td><td class="num %s"><strong>%.1f%%</strong></td></tr>',
          gsub("^R/", "", file_stats$File[i]),
          file_stats$Lines[i],
          file_stats$Covered[i],
          cls,
          pct
        )
      }, character(1)), collapse = "\n")
    )
    writeLines(html_content, html_file)
    return(invisible(html_file))
  }

  html_content <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Test Coverage - CausalMixGPD</title>
  <link href="../deps/bootstrap-5.3.1/bootstrap.min.css" rel="stylesheet">
  <link href="../extra.css" rel="stylesheet">
  <style>
    body { padding-top: 70px; }
    .coverage-badge { font-size: 1.2em; }
    .summary-table { max-width: 400px; }
    .file-table { font-size: 0.9em; }
    .btn-report { margin: 1em 0; }
  </style>
</head>
<body>
  <nav class="navbar navbar-expand-lg fixed-top">
    <div class="container">
      <a class="navbar-brand" href="../index.html">CausalMixGPD</a>
      <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarNav">
        <span class="navbar-toggler-icon"></span>
      </button>
      <div class="collapse navbar-collapse" id="navbarNav">
        <ul class="navbar-nav">
          <li class="nav-item"><a class="nav-link" href="../index.html">Home</a></li>
          <li class="nav-item"><a class="nav-link" href="../articles/manual-index.html">Manual</a></li>
          <li class="nav-item"><a class="nav-link" href="../articles/kernels-index.html">Kernels</a></li>
          <li class="nav-item"><a class="nav-link" href="../articles/examples-index.html">Examples</a></li>
          <li class="nav-item"><a class="nav-link active" href="index.html">Coverage</a></li>
          <li class="nav-item"><a class="nav-link" href="../reference/index.html">Reference</a></li>
        </ul>
        <ul class="navbar-nav ms-auto">
          <li class="nav-item"><a class="nav-link" href="https://github.com/arnabaich96/CausalMixGPD">GitHub</a></li>
        </ul>
      </div>
    </div>
  </nav>

  <main class="container py-4">
    <h1>Test Coverage Report</h1>

    <p class="coverage-badge">
      <img src="https://img.shields.io/badge/coverage-%%.1f%%%%25-%s" alt="Coverage: %%.1f%%%%">
    </p>

    <p><strong>Generated:</strong> %s</p>
    <p><strong>Sources:</strong> %s</p>

    <hr>

    <h2>Summary</h2>
    <table class="table table-striped summary-table">
      <tbody>
        <tr><td>Overall Coverage</td><td class="text-end"><strong>%%.1f%%%%</strong></td></tr>
        <tr><td>Total Lines</td><td class="text-end">%%d</td></tr>
        <tr><td>Covered Lines</td><td class="text-end">%%d</td></tr>
        <tr><td>Files Analyzed</td><td class="text-end">%%d</td></tr>
      </tbody>
    </table>

    <hr>

    <h2>Coverage Artifacts</h2>
    <p>
      <a href="report.html" class="btn btn-primary btn-report">View Full Interactive Report</a>
    </p>
    <p>The interactive report provides line-by-line coverage details for each file.</p>
    <ul>
      <li><a href="coverage_status.json"><code>coverage_status.json</code></a> - machine-readable summary</li>
      <li><a href="unused_functions.md"><code>unused_functions.md</code></a> - functions with 0%%%% coverage</li>
    </ul>

    <hr>

    <h2>File-by-File Coverage</h2>
    <table class="table table-striped table-hover file-table">
      <thead>
        <tr>
          <th>File</th>
          <th class="text-end">Lines</th>
          <th class="text-end">Covered</th>
          <th class="text-end">Coverage</th>
        </tr>
      </thead>
      <tbody>
%%s
      </tbody>
    </table>

    <hr>

    <p class="text-muted">
      <em>Coverage is calculated using <a href="https://covr.r-lib.org/">covr</a>.</em>
    </p>
  </main>

  <script src="../deps/bootstrap-5.3.1/bootstrap.bundle.min.js"></script>
  <script src="../extra.js"></script>
</body>
</html>',
                          badge_color,
                          display_time, sources_display
  )

  # Now do the numeric substitutions
  html_content <- sprintf(html_content,
                          percent, percent,
                          percent, sum(file_stats$Lines), sum(file_stats$Covered), nrow(file_stats),
                          paste(file_rows, collapse = "\n")
  )

  writeLines(html_content, html_file)
}

# ============================================================================
# Print usage when sourced
# ============================================================================

cat("CausalMixGPD Coverage Tools loaded.\n\n")
cat("Available functions:\n")
cat("  calculate_coverage(sources, test_level)  - Calculate coverage\n")
cat("  coverage_progress(test_level)            - Test coverage with progress\n")
cat("  coverage_report(sources, output_dir)     - Generate local HTML report\n")
cat("  coverage_upload(sources, token)          - Upload to Codecov\n")
cat("\nDefault sources: tests (CI-level)\n")
cat("Valid sources: 'tests', 'examples', 'vignettes', 'all'\n\n")
cat("Examples:\n")
cat("  coverage_report()                        # CI-level tests report to docs/coverage\n")
cat("  coverage_progress()                      # Progress reporter run\n")
cat("  coverage_report(sources = 'tests')       # Tests only (fastest)\n")
cat("  coverage_report(sources = 'all')         # All sources\n")
cat("  coverage_upload()                        # Upload CI-level tests coverage to Codecov\n")
cat("  coverage_upload(require_token = TRUE)    # Upload (fail if no token)\n")

# If executed via Rscript (for example through tools/coverage.bat), run the
# default CI-level tests coverage pipeline automatically.
args <- commandArgs(trailingOnly = FALSE)
file_args <- grep("^--file=", args, value = TRUE)
is_rscript <- any(grepl("coverage\\.R$", sub("^--file=", "", file_args), ignore.case = TRUE))
if (is_rscript) {
  cat("\nRunning default coverage pipeline (sources='tests', test_level='ci').\n")
  coverage_report(
    sources = "tests",
    test_level = "ci",
    output_dir = "docs/coverage",
    browse = FALSE
  )
  cat("\nCoverage report generation finished (local artifacts only).\n")
  cat("Use coverage_upload() explicitly if you want to push to Codecov.\n")
}
