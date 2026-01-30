# tools/update_namespace.R
# ----------------------------
# Regenerate NAMESPACE and man/ from roxygen comments. Do NOT edit NAMESPACE
# by hand; use devtools::document() (or this script) so exports stay in sync
# with @export tags in R/*.R.
#
# Usage: Rscript tools/update_namespace.R
#   or from R: devtools::document()
#

cat("Regenerating NAMESPACE and man/ via devtools::document()...\n")

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required. Install with install.packages('devtools').", call. = FALSE)
}

# Assume we are run from package root (e.g. R CMD BATCH from pkg root, or
# source() from R with setwd to pkg root)
pkg_root <- if (file.exists("DESCRIPTION")) getwd() else "."
if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
  stop("Cannot find package root (DESCRIPTION). Run from package directory.", call. = FALSE)
}

devtools::document(pkg = pkg_root)

cat("Done. NAMESPACE and man/ updated from @export and roxygen in R/.\n")
