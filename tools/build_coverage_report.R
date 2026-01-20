if (!nzchar(Sys.getenv("CODECOV_TOKEN"))) {
  stop("CODECOV_TOKEN is not set. Restart RStudio or set it via setx.")
}

# Set test level to "cran" to skip problematic tests during codecov runs
Sys.setenv(DPMIXGPD_TEST_LEVEL = "cran")
Sys.setenv(COVERAGE = "1")

# Calculate and upload coverage - continue even if some tests fail
cat("Calculating coverage (some test failures are acceptable)...\n")
cat("This may take several minutes...\n")

result <- tryCatch({
  # Calculate coverage - skip vignettes to avoid build issues
  cat("Step 1: Calculating package coverage (skipping vignettes)...\n")
  # Use type="tests" to skip vignettes, or "all" but with better error handling
  cov <- covr::package_coverage(
    type = "tests",  # Only test coverage, skip vignettes
    quiet = FALSE,
    pre_clean = FALSE
  )
  
  # Upload coverage even if there were test failures
  cat("Step 2: Uploading coverage to codecov...\n")
  covr::codecov(coverage = cov, quiet = FALSE)
  TRUE
}, error = function(e) {
  cat("Warning: Coverage calculation had issues, but attempting upload anyway...\n")
  cat("Error details: ", conditionMessage(e), "\n")
  
  # Try to upload whatever coverage we have using codecov() directly
  # This will recalculate but might work better
  tryCatch({
    cat("Attempting direct codecov upload (will recalculate coverage)...\n")
    # Try with tests only to avoid vignette issues
    options(covr.gcov = NULL)
    covr::codecov(quiet = FALSE)
    TRUE
  }, error = function(e2) {
    cat("Failed to upload coverage: ", conditionMessage(e2), "\n")
    cat("This is non-fatal - coverage upload is optional.\n")
    FALSE
  })
})

if (result) {
  cat("Coverage upload completed successfully!\n")
} else {
  cat("Coverage upload had issues, but this is non-fatal.\n")
  # Don't stop - allow the script to complete
}

