## Script to rebuild the Quarto landing site and pkgdown reference docs.
## Run from the repo root: `Rscript tools/build_site.R`

# locate the Quarto CLI so we can render the dedicated site directory
quarto_bin <- Sys.which("quarto")
if (!nzchar(quarto_bin)) {
  stop("quarto executable not found on PATH; install Quarto or set PATH accordingly.")
}

message("Rendering Quarto site...")
quarto_res <- system2(quarto_bin, c("render", "site"), stdout = "", stderr = "")
if (quarto_res != 0) {
  stop("Quarto render failed (status ", quarto_res, ").")
}

message("Running pkgdown clean/build...")
if (!requireNamespace("pkgdown", quietly = TRUE)) {
  stop("pkgdown is not installed in the current R library.")
}

pkgdown::clean_site(force = TRUE)
pkgdown::build_site()

message("Sites rebuilt: docs/ (Quarto) and docs/reference/ (pkgdown).")
