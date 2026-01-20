if (!nzchar(Sys.getenv("CODECOV_TOKEN"))) {
  stop("CODECOV_TOKEN is not set. Restart RStudio or set it via setx.")
}

# Set test level to "ci" to run MCMC-dependent tests for comprehensive coverage
Sys.setenv(DPMIXGPD_TEST_LEVEL = "ci")
Sys.setenv(COVERAGE = "1")

# Unload the package if it's already loaded to avoid covr instrumentation conflicts
# This prevents the "'from' must be a finite number" error
if (isNamespaceLoaded("DPmixGPD")) {
  cat("Unloading DPmixGPD namespace for clean coverage calculation...\n")
  try(unloadNamespace("DPmixGPD"), silent = TRUE)
}

# Calculate and upload coverage - continue even if some tests fail
cat("Calculating coverage (some test failures are acceptable)...\n")
cat("This may take several minutes...\n")

result <- tryCatch({
  # Calculate coverage using type = "none" with custom test code that tolerates failures
  # Using test_dir with stop_on_failure = FALSE and minimal reporter
  cat("Step 1: Calculating package coverage...\n")
  cov <- covr::package_coverage(
    type = "none",
    code = 'testthat::test_dir("tests/testthat", stop_on_failure = FALSE, reporter = "minimal")',
    quiet = FALSE,
    pre_clean = TRUE
  )
  
  # Upload coverage even if there were test failures
  cat("Step 2: Uploading coverage to codecov...\n")
  covr::codecov(coverage = cov, quiet = FALSE)
  TRUE
}, error = function(e) {
  cat("Warning: Primary coverage method had issues.\n")
  cat("Error details: ", conditionMessage(e), "\n")
  cat("Attempting alternative coverage method...\n\n")
  
  # Fallback: try with simpler test execution  
  tryCatch({
    cat("Attempting coverage with simpler test execution...\n")
    cov <- covr::package_coverage(
      type = "none",
      code = 'for(f in list.files("tests/testthat", pattern = "^test.*\\\\.R$", full.names = TRUE)) try(source(f), silent = TRUE)',
      quiet = FALSE,
      pre_clean = TRUE
    )
    covr::codecov(coverage = cov, quiet = FALSE)
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

