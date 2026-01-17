#!/usr/bin/env Rscript
cat("Loading package...\n")
devtools::load_all(quiet = TRUE)

cat("\n=== Building pkgdown site ===\n")
tryCatch({
  pkgdown::build_site(preview = FALSE)
  cat("\n✓ Build completed successfully\n")
}, error = function(e) {
  cat("\n✗ Build error:\n")
  print(e)
})
