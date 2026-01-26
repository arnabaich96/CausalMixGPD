# Fix renv version message
# Run this in R: source("fix_renv_version.R")

cat("Fixing renv version record...\n\n")

# Record the current renv version in the lockfile
# This will stop the informational message
renv::record("renv@1.1.6")

cat("✓ renv version recorded in lockfile\n")
cat("  The version message should no longer appear\n\n")

# Verify it worked
cat("Verifying renv status...\n")
renv::status()
