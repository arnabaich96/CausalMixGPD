# Build pkgdown site with lazy loading
options(timeout = 600)

cat("Building pkgdown site with lazy = TRUE...\n")
cat("This may take several minutes as vignettes are rendered.\n\n")

pkgdown::build_site(
  lazy = TRUE,
  preview = FALSE,
  quiet = FALSE
)

cat("\n✅ Site build complete!\n")
cat("Check docs/ folder for HTML output.\n")
