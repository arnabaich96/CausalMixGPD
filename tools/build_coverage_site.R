# ============================================================================
# Build Coverage Report for pkgdown Website
# ============================================================================
# This script generates both a full interactive HTML coverage report and an
# HTML summary page for the pkgdown site.
#
# Usage:
#   source("tools/build_coverage_site.R")
#
# Output files (in docs/coverage/ and pkgdown/assets/coverage/):
#   - index.html            Summary page with badge and file table
#   - report.html           Full interactive covr report
#   - coverage_status.json  JSON summary data
# ============================================================================

# Check required packages
if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Package 'covr' is required. Install with: install.packages('covr')")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install with: install.packages('jsonlite')")
}

# Configuration
output_dir <- "docs/coverage"
assets_dir <- "pkgdown/assets/coverage"
cat("Building coverage report...\n")
cat("Output directory:", output_dir, "\n")
cat("Assets directory:", assets_dir, "\n\n")

# Create output directories if needed
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cat("Created directory:", output_dir, "\n")
}
if (!dir.exists(assets_dir)) {
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)
  cat("Created directory:", assets_dir, "\n")
}

# ============================================================================
# Step 1: Calculate package coverage
# ============================================================================
cat("Step 1/4: Calculating package coverage (this may take a few minutes)...\n")

# Set test level to avoid problematic tests during coverage
Sys.setenv(DPMIXGPD_TEST_LEVEL = "cran")
Sys.setenv(COVERAGE = "1")

# Unload the package if it's already loaded to avoid covr instrumentation conflicts
# This prevents the "'from' must be a finite number" error that occurs when
# the package was previously loaded via devtools::load_all()
if (isNamespaceLoaded("DPmixGPD")) {
  cat("Unloading DPmixGPD namespace for clean coverage calculation...\n")
  try(unloadNamespace("DPmixGPD"), silent = TRUE)
}

# Calculate coverage using type = "none" with custom test code that tolerates failures
# This approach allows coverage to complete even when some tests fail
# We use try() to ensure test failures don't abort coverage calculation
cov <- tryCatch({
  covr::package_coverage(
    type = "none",
    code = "try(testthat::test_local(stop_on_failure = FALSE), silent = TRUE)",
    quiet = FALSE,
    pre_clean = TRUE
  )
}, error = function(e) {
  cat("\nWarning: Primary coverage method encountered an error.\n")
  cat("Error: ", conditionMessage(e), "\n")
  cat("Attempting alternative coverage method with type='tests'...\n\n")
  
 # Fallback: try standard type = "tests" approach
  # This may fail if tests fail, but gives more accurate coverage if it succeeds
  tryCatch({
    covr::package_coverage(
      type = "tests",
      quiet = FALSE,
      pre_clean = TRUE
    )
  }, error = function(e2) {
    cat("Alternative method also failed.\n")
    cat("Error: ", conditionMessage(e2), "\n")
    NULL
  })
})

if (is.null(cov)) {
  # Create a placeholder report if coverage failed completely
  cat("Coverage calculation failed. Creating placeholder report.\n")
  
  # Create minimal placeholder files
  placeholder_md <- c(
    "# Test Coverage Report",
    "",
    "**Status:** Coverage calculation failed",
    "",
    "Please run the coverage script locally to generate the full report.",
    "",
    "```r",
    "source('tools/build_coverage_site.R')",
    "```"
  )
  writeLines(placeholder_md, file.path(output_dir, "index.md"))
  
  placeholder_json <- list(
    percent = NA,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = "failed"
  )
  jsonlite::write_json(placeholder_json, file.path(output_dir, "coverage_status.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  
  cat("\nPlaceholder files created. Please resolve test failures and re-run.\n")
  stop("Coverage calculation failed", call. = FALSE)
}

cat("Coverage calculation complete.\n\n")

# ============================================================================
# Step 2: Generate full interactive HTML report
# ============================================================================
cat("Step 2/4: Generating interactive HTML report...\n")

report_file <- file.path(output_dir, "report.html")
covr::report(cov, file = report_file, browse = FALSE)
cat("Saved:", report_file, "\n")

# Also copy to pkgdown assets for proper inclusion in site build
assets_report <- file.path(assets_dir, "report.html")
file.copy(report_file, assets_report, overwrite = TRUE)
cat("Copied to:", assets_report, "\n\n")

# ============================================================================
# Step 3: Extract coverage statistics
# ============================================================================
cat("Step 3/4: Extracting coverage statistics...\n")

# Overall percentage
percent <- covr::percent_coverage(cov)

# Per-file coverage using tally_coverage
file_cov <- covr::tally_coverage(cov, by = "file")

# Aggregate by file
file_stats <- aggregate(
  cbind(value, covered = value > 0) ~ filename,
  data = file_cov,
  FUN = sum
)
file_stats$percent <- round(100 * file_stats$covered / file_stats$value, 1)
file_stats <- file_stats[order(-file_stats$percent, file_stats$filename), ]
names(file_stats) <- c("File", "Lines", "Covered", "Percent")

# Create summary data
timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
summary_data <- list(

  percent = round(percent, 2),
  timestamp = timestamp,

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

# Save JSON to both locations
json_file <- file.path(output_dir, "coverage_status.json")
jsonlite::write_json(summary_data, json_file, auto_unbox = TRUE, pretty = TRUE)
cat("Saved:", json_file, "\n")

assets_json <- file.path(assets_dir, "coverage_status.json")
file.copy(json_file, assets_json, overwrite = TRUE)
cat("Copied to:", assets_json, "\n\n")

# ============================================================================
# Step 4: Generate HTML summary page
# ============================================================================
cat("Step 4/4: Generating HTML summary page...\n")

# Badge color based on percentage
badge_color <- if (percent >= 80) {
  "brightgreen"
} else if (percent >= 60) {
  "yellow"
} else if (percent >= 40) {
  "orange"
} else {
  "red"
}

# Format timestamp for display
display_time <- format(
  as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  "%B %d, %Y at %H:%M UTC"
)

# Build file table rows
file_rows <- vapply(seq_len(nrow(file_stats)), function(i) {
  display_file <- gsub("^R/", "", file_stats$File[i])
  pct <- file_stats$Percent[i]
  
  # Color class based on percentage
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

# Build HTML content (standalone page matching pkgdown style with custom CSS)
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
          <li class="nav-item"><a class="nav-link" href="../articles/legacy-index.html">Legacy</a></li>
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
      <img src="https://img.shields.io/badge/coverage-%.1f%%25-%s" alt="Coverage: %.1f%%">
    </p>
    
    <p><strong>Generated:</strong> %s</p>
    
    <hr>
    
    <h2>Summary</h2>
    <table class="table table-striped summary-table">
      <tbody>
        <tr><td>Overall Coverage</td><td class="text-end"><strong>%.1f%%</strong></td></tr>
        <tr><td>Total Lines</td><td class="text-end">%d</td></tr>
        <tr><td>Covered Lines</td><td class="text-end">%d</td></tr>
        <tr><td>Files Analyzed</td><td class="text-end">%d</td></tr>
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
%s
      </tbody>
    </table>
    
    <hr>
    
    <p class="text-muted">
      <em>Coverage is calculated using <a href="https://covr.r-lib.org/">covr</a> and excludes test files.</em>
    </p>
  </main>

  <script src="../deps/bootstrap-5.3.1/bootstrap.bundle.min.js"></script>
  <script src="../extra.js"></script>
</body>
</html>',
  percent, badge_color, percent,
  display_time,
  percent, sum(file_stats$Lines), sum(file_stats$Covered), nrow(file_stats),
  paste(file_rows, collapse = "\n")
)

# Write HTML file to both locations
html_file <- file.path(output_dir, "index.html")
writeLines(html_content, html_file)
cat("Saved:", html_file, "\n")

assets_html <- file.path(assets_dir, "index.html")
file.copy(html_file, assets_html, overwrite = TRUE)
cat("Copied to:", assets_html, "\n\n")

# ============================================================================
# Done
# ============================================================================
cat("============================================================\n")
cat("Coverage report generated successfully!\n")
cat("============================================================\n")
cat("\n")
cat("Output files:\n")
cat("  - ", html_file, " (summary page)\n", sep = "")
cat("  - ", report_file, " (interactive report)\n", sep = "")
cat("  - ", json_file, " (JSON data)\n", sep = "")
cat("\n")
cat("Overall coverage: ", round(percent, 1), "%\n", sep = "")
cat("\n")
cat("Next steps:\n")
cat("  1. Rebuild pkgdown site: pkgdown::build_site()\n")
cat("  2. Commit docs/coverage/ changes\n")
cat("  3. Push to deploy\n")
