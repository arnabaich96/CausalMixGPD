# tools/render_quarto_safe.R
# Safe Quarto render with manual CSS handling

render_quarto_safe <- function(verbose = TRUE) {
  msg <- function(...) if (isTRUE(verbose)) message(...)

  # Find project root
  if (!requireNamespace("rprojroot", quietly = TRUE)) {
    stop("Package 'rprojroot' is required but not installed.")
  }

  root <- rprojroot::find_root(rprojroot::has_file("_quarto.yml"))
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  msg("Project root: ", root)

  # Check Quarto
  quarto_exe <- Sys.which("quarto")
  if (identical(quarto_exe, "")) stop("Quarto CLI not found on PATH.")

  # Ensure clean output dir
  quarto_out <- file.path(root, "quarto")
  if (dir.exists(quarto_out)) {
    unlink(quarto_out, recursive = TRUE, force = TRUE)
    Sys.sleep(1)
  }
  dir.create(quarto_out, recursive = TRUE, showWarnings = FALSE)
  msg("Created clean quarto output directory: ", quarto_out)

  # Render with Quarto
  msg("Running Quarto render...")
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(root)

  result <- system2(
    quarto_exe,
    c("render", root, "--output-dir", quarto_out),
    stdout = TRUE,
    stderr = TRUE
  )

  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    msg("Quarto render failed with status: ", attr(result, "status"))
    cat(paste(result, collapse = "\n"))
    stop("Quarto render failed")
  }

  msg("Quarto render completed successfully")

  # Copy CSS files to output
  website_dir <- file.path(root, "website")
  quarto_website_dir <- file.path(quarto_out, "website")
  dir.create(quarto_website_dir, recursive = TRUE, showWarnings = FALSE)

  css_files <- c("styles.css", "theme.css")
  for (css in css_files) {
    src <- file.path(website_dir, css)
    dst <- file.path(quarto_website_dir, css)
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE, copy.mode = FALSE)
      msg("Copied ", css, " to output")
    }
  }

  msg("Quarto render complete!")
  invisible(TRUE)
}

if (!interactive()) {
  render_quarto_safe(verbose = TRUE)
}
