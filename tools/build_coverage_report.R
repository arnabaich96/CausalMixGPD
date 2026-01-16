# Generate an HTML coverage report for pkgdown hosting.
if (!requireNamespace("covr", quietly = TRUE)) {
  stop("The 'covr' package is required. Install with install.packages('covr').", call. = FALSE)
}

# Ensure HTML report dependencies are available
needed <- c("DT", "htmltools")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages for HTML report: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

message("Computing coverage ...")
cov <- covr::package_coverage()

out_dir <- file.path("docs", "coverage")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(out_dir, "index.html")
message("Writing coverage report to ", out_file)

covr::report(cov, file = out_file, browse = FALSE)
message("Coverage report complete.")
