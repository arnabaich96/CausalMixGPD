# Quick script to verify renv setup
# Run this in R: source("verify_renv.R")

cat("=== renv Status Check ===\n\n")

# Check if renv is loaded
if ("renv" %in% loadedNamespaces()) {
  cat("✓ renv is loaded\n")
  cat("  Version:", as.character(packageVersion("renv")), "\n")
} else {
  cat("✗ renv is NOT loaded\n")
  cat("  Try: source('renv/activate.R')\n")
}

# Check renv status
if ("renv" %in% loadedNamespaces()) {
  cat("\n=== renv::status() ===\n")
  tryCatch(
    renv::status(),
    error = function(e) {
      cat("Error checking status:", conditionMessage(e), "\n")
    }
  )
}

# Check R version
cat("\n=== R Version ===\n")
cat("Current:", as.character(getRversion()), "\n")
cat("Lockfile expects: 4.5.2\n")

if (as.character(getRversion()) == "4.5.2") {
  cat("✓ R version matches lockfile\n")
} else {
  cat("⚠ R version does NOT match lockfile\n")
  cat("  This is usually fine, but some packages may need recompilation\n")
}

cat("\n=== Project Library ===\n")
lib_path <- renv::paths$library()
if (dir.exists(lib_path)) {
  cat("✓ Project library exists:", lib_path, "\n")
  pkgs <- list.files(lib_path)
  cat("  Packages installed:", length(pkgs), "\n")
} else {
  cat("✗ Project library not found\n")
  cat("  Run: renv::restore()\n")
}

cat("\n=== Summary ===\n")
if ("renv" %in% loadedNamespaces() && dir.exists(renv::paths$library())) {
  cat("✓ renv appears to be set up correctly!\n")
  cat("  The version message you saw is just informational.\n")
} else {
  cat("⚠ renv needs attention. See messages above.\n")
}
