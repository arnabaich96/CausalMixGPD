# ============================================================================
# Unified Coverage Calculation and Reporting for DPmixGPD
# ============================================================================
#
# This script provides functions for calculating package coverage and generating
# reports. It supports multiple coverage sources: tests, examples, and vignettes.
#
# Usage:
#   source("tools/coverage.R")
#
#   # Generate local HTML report (default: tests + examples)
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

# Files to exclude entirely from coverage
.coverage_line_exclusions <- function(path = ".") {
  files <- c(
    "R/Utility.R",
    "R/00-kernel-registry.R",
    "R/globals.R"
  )
  full_paths <- normalizePath(file.path(path, files), winslash = "/", mustWork = FALSE)
  all_paths <- unique(c(files, full_paths))
  stats::setNames(as.list(rep(Inf, length(all_paths))), all_paths)
}

# Get Codecov token from codecov.yml
.get_codecov_token <- function() {
  yaml_file <- "codecov.yml"
  if (file.exists(yaml_file)) {
    lines <- readLines(yaml_file, warn = FALSE)
    lines <- sub("\\s+#.*$", "", lines)

    codecov_idx <- grep("^\\s*codecov\\s*:", lines)
    if (length(codecov_idx) > 0) {
      base_indent <- attr(regexpr("^\\s*", lines[codecov_idx[1]]), "match.length")
      for (i in seq.int(codecov_idx[1] + 1L, length(lines))) {
        line <- lines[i]
        if (!nzchar(trimws(line))) next
        indent <- attr(regexpr("^\\s*", line), "match.length")
        if (indent <= base_indent) break
        if (grepl("^\\s*token\\s*:", line)) {
          token <- trimws(sub("^\\s*token\\s*:\\s*", "", line))
          if (nzchar(token)) return(token)
        }
      }
    }

    token_line <- grep("^\\s*token\\s*:", lines, value = TRUE)
    if (length(token_line) > 0) {
      token <- trimws(sub("^\\s*token\\s*:\\s*", "", token_line[1]))
      if (nzchar(token)) return(token)
    }
  }

  return("")
}

# ============================================================================
# calculate_coverage: Core function to calculate package coverage
# ============================================================================

#' Calculate package coverage
#'
#' @param sources Character vector specifying coverage sources.
#'   Options: "tests", "examples", "vignettes", or "all".
#'   Default: c("tests", "examples")
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
    sources = c("tests", "examples"),
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

  if (isNamespaceLoaded("DPmixGPD")) {
    if (!quiet) cat("Unloading DPmixGPD namespace for clean coverage calculation...\n")
    try(unloadNamespace("DPmixGPD"), silent = TRUE)
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

  # Fallback if primary method failed

if (is.null(cov) && "tests" %in% sources) {
    if (!quiet) cat("Attempting fallback coverage method...\n")
    cov <- tryCatch({
      covr::package_coverage(
        type = "none",
        code = 'for(f in list.files("tests/testthat", pattern = "^test.*\\\\.R$", full.names = TRUE)) try(source(f), silent = TRUE)',
        quiet = quiet,
        pre_clean = TRUE,
        line_exclusions = .coverage_line_exclusions()
      )
    }, error = function(e2) {
      if (!quiet) cat("Fallback also failed:", conditionMessage(e2), "\n")
      NULL
    })
  }

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
    code = 'testthat::test_dir("tests/testthat", stop_on_failure = FALSE, reporter = "minimal")',
    quiet = quiet,
    pre_clean = TRUE,
    line_exclusions = .coverage_line_exclusions()
  )
}

# Internal: Calculate combined coverage from multiple sources
.calculate_combined_coverage <- function(sources, quiet = FALSE) {
  # For combined sources, we need to handle tests specially due to nimble issues
  # Try using covr's built-in types for non-test sources

  non_test_sources <- setdiff(sources, "tests")

  if (length(non_test_sources) > 0 && "tests" %in% sources) {
    # Try combined approach first
    tryCatch({
      covr::package_coverage(
        type = non_test_sources,
        code = 'testthat::test_dir("tests/testthat", stop_on_failure = FALSE, reporter = "minimal")',
        quiet = quiet,
        pre_clean = TRUE,
        line_exclusions = .coverage_line_exclusions()
      )
    }, error = function(e) {
      # Fall back to tests only if combined fails
      if (!quiet) cat("Combined coverage failed, falling back to tests only.\n")
      .calculate_tests_coverage(quiet = quiet)
    })
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

# ============================================================================
# coverage_report: Generate local HTML coverage report
# ============================================================================

#' Generate local HTML coverage report
#'
#' Calculates coverage and generates an interactive HTML report for the
#' pkgdown website.
#'
#' @param sources Character vector specifying coverage sources.
#'   Default: c("tests", "examples")
#' @param test_level Test tier: "cran", "ci", or "full". Default: "ci"
#' @param output_dir Output directory. Default: "docs/coverage"
#' @param browse Logical; open report in browser? Default: FALSE
#' @return Invisibly returns the coverage object
#'
#' @examples
#' \dontrun{
#' # Default: tests + examples
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
    sources = c("tests", "examples"),
    test_level = "ci",
    output_dir = "docs/coverage",
    browse = FALSE
) {
  .check_coverage_deps()
  resolved <- .resolve_coverage_inputs(sources, test_level)
  sources <- resolved$sources
  test_level <- resolved$test_level

  assets_dir <- "pkgdown/assets/coverage"

  cat("============================================================\n")
  cat("Building Coverage Report\n")
  cat("============================================================\n")
  cat("Sources:", paste(sources, collapse = ", "), "\n")
  cat("Test level:", test_level, "\n")
  cat("Output directory:", output_dir, "\n\n")

  # Create output directories
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    cat("Created directory:", output_dir, "\n")
  }
  if (!dir.exists(assets_dir)) {
    dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)
    cat("Created directory:", assets_dir, "\n")
  }

  # Step 1: Calculate coverage
  cat("\nStep 1/5: Calculating package coverage...\n")
  cov <- calculate_coverage(sources = sources, test_level = test_level, quiet = FALSE)

  if (is.null(cov)) {
    .create_placeholder_report(output_dir, sources)
    stop("Coverage calculation failed", call. = FALSE)
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
  cat("\nNext steps:\n")
  cat("  1. Rebuild pkgdown site: pkgdown::build_site()\n")
  cat("  2. Commit docs/coverage/ changes\n")
  cat("  3. Push to deploy\n")

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
#' 3. The local file `tools/.codecov_token` (git-ignored)
#'
#' @param sources Character vector specifying coverage sources.
#'   Default: c("tests", "examples")
#' @param test_level Test tier: "cran", "ci", or "full". Default: "ci"
#' @param token Codecov token. Default: auto-detected from env var or token file
#' @param require_token Logical; if TRUE, throw an error when token is missing.
#'   If FALSE, skip upload gracefully. Default: FALSE
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
    sources = c("tests", "examples"),
    test_level = "ci",
    token = .get_codecov_token(),
    require_token = FALSE,
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
    if (!quiet) cat("Step 1: Calculating package coverage...\n")
    cov <- calculate_coverage(sources = sources, test_level = test_level, quiet = quiet)

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
    "source('tools/coverage.R')",
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

  html_content <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Test Coverage - DPmixGPD</title>
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
      <a class="navbar-brand" href="../index.html">DPmixGPD</a>
      <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarNav">
        <span class="navbar-toggler-icon"></span>
      </button>
      <div class="collapse navbar-collapse" id="navbarNav">
        <ul class="navbar-nav">
          <li class="nav-item"><a class="nav-link" href="../index.html">Home</a></li>
          <li class="nav-item"><a class="nav-link" href="../articles/manual-index.html">Manual</a></li>
          <li class="nav-item"><a class="nav-link" href="../articles/kernels-index.html">Kernels</a></li>
          <li class="nav-item"><a class="nav-link" href="../articles/cookbook-index.html">Cookbook</a></li>
          <li class="nav-item"><a class="nav-link active" href="index.html">Coverage</a></li>
          <li class="nav-item"><a class="nav-link" href="../reference/index.html">Reference</a></li>
        </ul>
        <ul class="navbar-nav ms-auto">
          <li class="nav-item"><a class="nav-link" href="https://github.com/arnabaich96/DPmixGPD">GitHub</a></li>
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

    <h2>Detailed Report</h2>
    <p>
      <a href="report.html" class="btn btn-primary btn-report">View Full Interactive Report</a>
    </p>
    <p>The interactive report provides line-by-line coverage details for each file.</p>

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

cat("DPmixGPD Coverage Tools loaded.\n\n")
cat("Available functions:\n")
cat("  calculate_coverage(sources, test_level)  - Calculate coverage\n")
cat("  coverage_report(sources, output_dir)     - Generate local HTML report\n")
cat("  coverage_upload(sources, token)          - Upload to Codecov\n")
cat("\nDefault sources: tests + examples\n")
cat("Valid sources: 'tests', 'examples', 'vignettes', 'all'\n\n")
cat("Examples:\n")
cat("  coverage_report()                        # Default report\n")
cat("  coverage_report(sources = 'tests')       # Tests only (fastest)\n")
cat("  coverage_report(sources = 'all')         # All sources\n")
cat("  coverage_upload()                        # Upload to Codecov (skips if no token)\n")
cat("  coverage_upload(require_token = TRUE)    # Upload (fail if no token)\n")
