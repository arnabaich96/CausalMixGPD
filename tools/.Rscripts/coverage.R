# ============================================================================
# Minimal canonical coverage pipeline for CausalMixGPD
# ============================================================================
#
# Canonical output:
#   covr/assets/
#
# Published mirror:
#   docs/coverage/
#
# Usage:
#   source("tools/.Rscripts/coverage.R")
#   coverage_report()
# ============================================================================

setwd(here::here())

.coverage_primary_dir <- function() {
  "covr/assets"
}

.coverage_default_output_dir <- function() {
  "docs/coverage"
}

.coverage_test_level <- function() {
  lvl <- Sys.getenv("DPMIXGPD_COVERAGE_LEVEL", unset = "ci")
  if (!nzchar(lvl)) {
    lvl <- "ci"
  }
  lvl
}

.coverage_modes <- function() {
  valid <- c("custom", "tests", "file")
  # Prefer file mode first because it avoids the opaque package-install phase
  # that can make local coverage runs look stalled on this repository.
  raw <- Sys.getenv("DPMIXGPD_COVERAGE_MODES", unset = "file,custom,tests")
  parts <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  modes <- unique(parts[nzchar(parts) & parts %in% valid])
  if (!length(modes)) {
    modes <- c("file", "custom", "tests")
  }
  modes
}

.coverage_ci_coverage_only <- function() {
  Sys.getenv("DPMIXGPD_CI_COVERAGE_ONLY", unset = "1")
}

.coverage_include_coverage_only_file <- function() {
  identical(.coverage_ci_coverage_only(), "1")
}

.check_coverage_deps <- function() {
  if (!requireNamespace("covr", quietly = TRUE)) {
    stop("Package 'covr' is required. Install with install.packages('covr').", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install with install.packages('jsonlite').", call. = FALSE)
  }
  invisible(TRUE)
}

.set_covr_install_opts <- function() {
  options(covr.install = c("--no-build-vignettes", "--no-manual"))
  options(covr.install_args = c("--no-build-vignettes", "--no-manual"))
  invisible(TRUE)
}

.coverage_install_opts <- function() {
  c("--no-build-vignettes", "--no-manual", "--no-html")
}

.coverage_clean_dir <- function(path) {
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.coverage_same_path <- function(path_a, path_b) {
  identical(
    normalizePath(path_a, winslash = "/", mustWork = FALSE),
    normalizePath(path_b, winslash = "/", mustWork = FALSE)
  )
}

.coverage_artifact_files <- function() {
  c("index.html", "report.html", "coverage_status.json", "unused_functions.md", "cobertura.xml")
}

.coverage_sync_dir <- function(source_dir, target_dir) {
  if (.coverage_same_path(source_dir, target_dir)) {
    return(invisible(target_dir))
  }

  .coverage_clean_dir(target_dir)

  artifact_files <- .coverage_artifact_files()
  copied <- file.copy(file.path(source_dir, artifact_files), target_dir, overwrite = TRUE)
  if (!all(copied)) {
    stop("Failed to mirror one or more coverage artifacts to ", target_dir, call. = FALSE)
  }

  source_lib <- file.path(source_dir, "lib")
  if (dir.exists(source_lib)) {
    copied_lib <- file.copy(source_lib, target_dir, recursive = TRUE, overwrite = TRUE)
    if (!isTRUE(copied_lib)) {
      stop("Failed to mirror covr HTML dependency directory to ", target_dir, call. = FALSE)
    }
  }

  invisible(target_dir)
}

.coverage_refresh_metadata <- function(path = ".") {
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    return(invisible(FALSE))
  }
  tryCatch({
    if (requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(path = path, export_all = FALSE, helpers = FALSE, quiet = TRUE)
      if (isNamespaceLoaded("CausalMixGPD")) {
        unloadNamespace("CausalMixGPD")
      }
    }
    TRUE
  }, error = function(e) FALSE)
}

.coverage_excluded_files <- function() {
  c("1-registry.R", "01-registry.R", "zzz.R")
}

.coverage_current_r_files <- function(path = ".") {
  files <- list.files(file.path(path, "R"), pattern = "\\.[Rr]$", full.names = FALSE)
  unique(c(files, .coverage_excluded_files()))
}

.coverage_source_files <- function(path = ".") {
  list.files(file.path(path, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
}

.coverage_selected_test_basenames <- function() {
  # Historical high-coverage test set restored from commit 5f60fe6.
  c(
    "test-ci-level-only.R",
    "test-ci.R",
    "test-coverage-heavy.R",
    "test-integration.R",
    "test-performance-acceptance.R",
    "test-performance-phase2-ess.R",
    "test-performance-phase2-predict.R",
    "test-performance-phase2-samplers.R",
    "test-performance-phase2-zupdate.R",
    "test-progress.R",
    "test-unit.R",
    "test_cluster_coverage_edges.R",
    "test_cluster_fit_predict.R",
    "test_cluster_methods.R",
    "test_cluster_ordering_summary.R"
  )
}

.coverage_test_files <- function(path = ".") {
  test_root <- file.path(path, "tests", "testthat")
  all_r <- list.files(test_root, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
  if (!length(all_r)) {
    stop("Coverage found no R test files under: ", test_root, call. = FALSE)
  }

  setup <- all_r[basename(all_r) == "setup.R"]
  helpers <- all_r[grepl("^helper.*\\.[Rr]$", basename(all_r))]
  tests <- all_r[grepl("^test.*\\.[Rr]$", basename(all_r))]
  if (!.coverage_include_coverage_only_file()) {
    tests <- tests[basename(tests) != "test-ci-level-only.R"]
  }
  tests <- tests[basename(tests) %in% .coverage_selected_test_basenames()]
  if (!length(tests)) {
    stop("Coverage found no matching canonical coverage test entrypoints.", call. = FALSE)
  }

  unique(c(setup, sort(helpers), sort(tests)))
}

.coverage_line_exclusions <- function(path = ".") {
  files <- file.path("R", .coverage_excluded_files())
  full_paths <- normalizePath(file.path(path, files), winslash = "/", mustWork = FALSE)
  all_paths <- unique(c(files, full_paths))
  stats::setNames(as.list(rep(Inf, length(all_paths))), all_paths)
}

.filter_coverage_object <- function(cov, path = ".") {
  if (is.null(cov) || !inherits(cov, "coverage")) {
    return(cov)
  }

  nm <- names(cov)
  if (is.null(nm) || !length(nm)) {
    return(cov)
  }

  keep_files <- .coverage_current_r_files(path = path)
  keep <- sub(":.*$", "", nm) %in% keep_files
  cov[keep]
}

.coverage_test_code <- function(progress = FALSE) {
  reporter_expr <- if (isTRUE(progress)) {
    "testthat::ProgressReporter$new(show_praise = FALSE)"
  } else {
    "testthat::ProgressReporter$new(show_praise = FALSE, update_interval = Inf)"
  }
  test_basenames <- basename(.coverage_test_files())
  test_basenames <- test_basenames[grepl("^test.*\\.[Rr]$", test_basenames)]
  test_expr <- paste(sprintf('"%s"', unique(test_basenames)), collapse = ", ")

  paste(
    "library(CausalMixGPD)",
    'pkg_tests <- system.file("tests", "testthat", package = "CausalMixGPD")',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) {',
    '  local_candidates <- c("tests/testthat", "./tests/testthat")',
    '  local_hit <- local_candidates[file.exists(local_candidates)]',
    '  if (length(local_hit) > 0L) pkg_tests <- normalizePath(local_hit[1], winslash = "/", mustWork = FALSE)',
    '}',
    'if (!nzchar(pkg_tests) || !dir.exists(pkg_tests)) stop("Coverage could not find tests/testthat")',
    sprintf(
      'Sys.setenv(COVERAGE = "1", DPMIXGPD_TEST_LEVEL = "%s", DPMIXGPD_CI_COVERAGE_ONLY = "%s")',
      .coverage_test_level(),
      .coverage_ci_coverage_only()
    ),
    sprintf('test_files <- file.path(pkg_tests, c(%s))', test_expr),
    'test_files <- test_files[file.exists(test_files)]',
    'if (length(test_files) == 0L) stop("Coverage found no test files under: ", pkg_tests)',
    sprintf('reporter <- %s', reporter_expr),
    'for (target in test_files) {',
    '  cat("== coverage: running ", basename(target), "\\n", sep = "")',
    '  flush.console()',
    '  testthat::test_file(target, reporter = reporter, package = "CausalMixGPD")',
    '}',
    sep = "\n"
  )
}

.copy_rout_fail <- function(msg) {
  rx <- regexec("`([^`]*testthat\\.Rout\\.fail)`", msg)
  m <- regmatches(msg, rx)
  rout_fail <- if (length(m) && length(m[[1]]) >= 2) m[[1]][[2]] else ""
  if (nzchar(rout_fail) && file.exists(rout_fail)) {
    dir.create(.coverage_primary_dir(), recursive = TRUE, showWarnings = FALSE)
    try(
      file.copy(rout_fail, file.path(.coverage_primary_dir(), "testthat.Rout.fail"), overwrite = TRUE),
      silent = TRUE
    )
  }
  invisible(rout_fail)
}

#' Calculate coverage using the canonical coverage entrypoints only
#'
#' @param quiet Logical; suppress covr output. Default: FALSE.
#' @return A covr coverage object.
calculate_coverage <- function(quiet = FALSE) {
  .check_coverage_deps()
  .set_covr_install_opts()

  Sys.setenv(
    DPMIXGPD_TEST_LEVEL = .coverage_test_level(),
    COVERAGE = "1",
    DPMIXGPD_CI_COVERAGE_ONLY = .coverage_ci_coverage_only(),
    DPMIXGPD_SKIP_COVR_CAUSAL_BRANCHES = Sys.getenv("DPMIXGPD_SKIP_COVR_CAUSAL_BRANCHES", unset = "1")
  )

  if (isNamespaceLoaded("CausalMixGPD")) {
    if (!quiet) {
      cat("Unloading CausalMixGPD namespace for clean coverage calculation...\n")
    }
    try(unloadNamespace("CausalMixGPD"), silent = TRUE)
  }

  if (!quiet) {
    cat("Calculating coverage from tests only.\n")
    cat("Test level:", .coverage_test_level(), "\n\n")
  }

  run_with_mode <- function(mode) {
    if (identical(mode, "file")) {
      covr::file_coverage(
        source_files = .coverage_source_files(),
        test_files = .coverage_test_files(),
        line_exclusions = .coverage_line_exclusions()
      )
    } else if (identical(mode, "custom")) {
      covr::package_coverage(
        type = "none",
        code = .coverage_test_code(progress = !quiet),
        quiet = quiet,
        pre_clean = TRUE,
        INSTALL_opts = .coverage_install_opts(),
        line_exclusions = .coverage_line_exclusions()
      )
    } else {
      covr::package_coverage(
        type = "tests",
        quiet = quiet,
        pre_clean = TRUE,
        INSTALL_opts = .coverage_install_opts(),
        line_exclusions = .coverage_line_exclusions()
      )
    }
  }

  attempt_messages <- character(0)
  cov <- NULL

  for (mode in .coverage_modes()) {
    if (!quiet) {
      cat("Coverage mode:", mode, "\n")
    }

    cov <- tryCatch(
      run_with_mode(mode),
      error = function(e) {
        msg <- conditionMessage(e)
        attempt_messages <<- c(attempt_messages, sprintf("%s: %s", mode, msg))
        .copy_rout_fail(msg)
        NULL
      }
    )

    if (!is.null(cov)) {
      break
    }

    if (!quiet) {
      cat("Mode '", mode, "' failed; trying fallback.\n", sep = "")
    }
  }

  cov <- .filter_coverage_object(cov)

  if (is.null(cov)) {
    if (!quiet && length(attempt_messages) > 0L) {
      cat("\nCoverage calculation failed in all modes.\n")
      cat(paste0(" - ", attempt_messages), sep = "\n")
      cat("\n")
    }
    stop("Coverage calculation failed.", call. = FALSE)
  }

  if (!quiet) {
    cat("\nCoverage calculation complete.\n")
    cat("Overall coverage:", round(covr::percent_coverage(cov), 1), "%\n\n")
  }

  cov
}

.extract_coverage_stats <- function(cov) {
  percent <- covr::percent_coverage(cov)
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
    pipeline = paste0("tests/", .coverage_test_level()),
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

.extract_unused_functions <- function(cov) {
  line_cov <- covr::tally_coverage(cov, by = "line")
  if (!is.data.frame(line_cov) || nrow(line_cov) == 0L) {
    return(list())
  }

  file_col <- if ("filename" %in% names(line_cov)) "filename" else if ("file" %in% names(line_cov)) "file" else NULL
  fun_col <- intersect(names(line_cov), c("function", "fun", "func"))
  value_col <- if ("value" %in% names(line_cov)) "value" else if ("covered" %in% names(line_cov)) "covered" else NULL

  if (is.null(file_col) || length(fun_col) == 0L || is.null(value_col)) {
    return(list())
  }

  fun_col <- fun_col[1]
  df <- data.frame(
    file = line_cov[[file_col]],
    func = line_cov[[fun_col]],
    value = line_cov[[value_col]],
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$func) & nzchar(df$func), , drop = FALSE]
  if (nrow(df) == 0L) {
    return(list())
  }

  df$total_line <- !is.na(df$value)
  df$covered_line <- df$value > 0

  agg <- aggregate(
    cbind(total_line, covered_line) ~ file + func,
    data = df,
    FUN = sum,
    na.rm = TRUE
  )
  unused <- agg[agg$covered_line == 0 & agg$total_line > 0, , drop = FALSE]
  if (nrow(unused) == 0L) {
    return(list())
  }

  unused <- unused[order(unused$file, unused$func), , drop = FALSE]
  split(unused$func, unused$file)
}

.write_unused_functions_report <- function(unused_by_file, output_file) {
  timestamp <- format(Sys.time(), "%B %d, %Y at %H:%M UTC", tz = "UTC")
  lines <- c(
    "# Unused Functions (0% covered)",
    "",
    paste("**Generated:**", timestamp),
    paste("**Pipeline:** tests /", .coverage_test_level()),
    ""
  )

  if (length(unused_by_file) == 0L) {
    lines <- c(lines, "All functions have non-zero coverage.")
  } else {
    for (file in names(unused_by_file)) {
      lines <- c(lines, paste0("## `", file, "`"), "")
      lines <- c(lines, paste0("- `", unused_by_file[[file]], "`"), "")
    }
  }

  writeLines(lines, output_file)
}

.generate_summary_html <- function(stats, html_file) {
  percent <- stats$percent
  file_stats <- stats$file_stats
  display_time <- format(
    as.POSIXct(stats$timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    "%B %d, %Y at %H:%M UTC"
  )

  badge_color <- if (percent >= 80) {
    "green"
  } else if (percent >= 60) {
    "yellow"
  } else if (percent >= 40) {
    "orange"
  } else {
    "red"
  }

  file_rows <- vapply(seq_len(nrow(file_stats)), function(i) {
    pct <- file_stats$Percent[i]
    cls <- if (pct >= 80) {
      "text-success"
    } else if (pct >= 60) {
      "text-warning"
    } else {
      "text-danger"
    }
    sprintf(
      '<tr><td><code>%s</code></td><td class="text-end">%d</td><td class="text-end">%d</td><td class="text-end %s"><strong>%.1f%%</strong></td></tr>',
      gsub("^R/", "", file_stats$File[i]),
      file_stats$Lines[i],
      file_stats$Covered[i],
      cls,
      pct
    )
  }, character(1))

  html <- sprintf(
'<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Coverage - CausalMixGPD</title>
  <link href="../deps/bootstrap-5.3.1/bootstrap.min.css" rel="stylesheet">
  <style>
    body { padding-top: 3rem; }
    .coverage-badge { font-size: 1.1rem; }
  </style>
</head>
<body>
  <main class="container py-4">
    <h1>Coverage Report</h1>
    <p class="coverage-badge">
      <img src="https://img.shields.io/badge/coverage-%.1f%%25-%s" alt="Coverage %.1f%%">
    </p>
    <p><strong>Generated:</strong> %s</p>
    <p><strong>Pipeline:</strong> tests / %s</p>
    <p>
      <a href="report.html" class="btn btn-primary">Open full interactive report</a>
    </p>
    <ul>
      <li><a href="coverage_status.json"><code>coverage_status.json</code></a></li>
      <li><a href="unused_functions.md"><code>unused_functions.md</code></a></li>
      <li><a href="cobertura.xml"><code>cobertura.xml</code></a></li>
    </ul>
    <table class="table table-striped table-hover">
      <thead>
        <tr>
          <th>File</th>
          <th class="text-end">Lines</th>
          <th class="text-end">Covered</th>
          <th class="text-end">Coverage</th>
        </tr>
      </thead>
      <tbody>
%s
      </tbody>
    </table>
  </main>
</body>
</html>',
    percent,
    badge_color,
    percent,
    display_time,
    .coverage_test_level(),
    paste(file_rows, collapse = "\n")
  )

  writeLines(html, html_file)
  invisible(html_file)
}

#' Build the canonical local coverage report
#'
#' Runs the canonical coverage entrypoints under covr, writes canonical artifacts to
#' `covr/assets`, and mirrors them to `docs/coverage`.
#'
#' @param output_dir Mirror directory for the published site.
#' @param browse Logical; open the mirrored summary page in a browser.
#' @return Invisibly returns the coverage object.
coverage_report <- function(output_dir = .coverage_default_output_dir(), browse = FALSE) {
  .check_coverage_deps()
  .set_covr_install_opts()

  assets_dir <- .coverage_primary_dir()
  mirror_dir <- output_dir

  .coverage_clean_dir(assets_dir)
  if (!.coverage_same_path(mirror_dir, assets_dir)) {
    .coverage_clean_dir(mirror_dir)
  }
  .coverage_refresh_metadata()

  cat("============================================================\n")
  cat("Building Coverage Report\n")
  cat("============================================================\n")
  cat("Pipeline: tests / ", .coverage_test_level(), "\n", sep = "")
  cat("Canonical output directory: ", assets_dir, "\n", sep = "")
  if (!.coverage_same_path(mirror_dir, assets_dir)) {
    cat("Published mirror directory: ", mirror_dir, "\n", sep = "")
  }
  cat("\n")

  cat("Step 1/6: Calculating coverage...\n")
  cov <- calculate_coverage(quiet = FALSE)

  cat("Step 2/6: Writing interactive HTML report...\n")
  report_file <- file.path(assets_dir, "report.html")
  covr::report(cov, file = report_file, browse = FALSE)

  cat("Step 3/6: Writing Cobertura XML...\n")
  cobertura_file <- file.path(assets_dir, "cobertura.xml")
  covr::to_cobertura(cov, filename = cobertura_file)

  cat("Step 4/6: Writing JSON summary...\n")
  stats <- .extract_coverage_stats(cov)
  json_file <- file.path(assets_dir, "coverage_status.json")
  jsonlite::write_json(stats$summary, json_file, auto_unbox = TRUE, pretty = TRUE)

  cat("Step 5/6: Writing unused-function report and summary page...\n")
  unused_file <- file.path(assets_dir, "unused_functions.md")
  .write_unused_functions_report(.extract_unused_functions(cov), unused_file)
  index_file <- file.path(assets_dir, "index.html")
  .generate_summary_html(stats, index_file)

  cat("Step 6/6: Mirroring artifacts to docs...\n")
  .coverage_sync_dir(assets_dir, mirror_dir)

  cat("\nCoverage report generated successfully.\n")
  cat("Canonical artifacts: ", assets_dir, "\n", sep = "")
  if (!.coverage_same_path(mirror_dir, assets_dir)) {
    cat("Published mirror: ", mirror_dir, "\n", sep = "")
  }
  cat("Overall coverage: ", round(stats$percent, 1), "%\n", sep = "")

  if (browse) {
    utils::browseURL(file.path(mirror_dir, "index.html"))
  }

  invisible(cov)
}

cat("CausalMixGPD coverage helper loaded.\n")
cat("Run `coverage_report()` to build the canonical local coverage report.\n")

args <- commandArgs(trailingOnly = FALSE)
file_args <- grep("^--file=", args, value = TRUE)
is_rscript <- any(grepl("coverage\\.R$", sub("^--file=", "", file_args), ignore.case = TRUE))
if (is_rscript) {
  cat("\nRunning canonical local coverage pipeline.\n")
  coverage_report()
  cat("\nCoverage report generation finished successfully.\n")
}
