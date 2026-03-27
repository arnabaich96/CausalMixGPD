#!/usr/bin/env Rscript

options(width = 70)

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install with install.packages('here').", call. = FALSE)
}

if (!requireNamespace("knitr", quietly = TRUE)) {
  stop("Package 'knitr' is required. Install with install.packages('knitr').", call. = FALSE)
}

if (!requireNamespace("tinytex", quietly = TRUE)) {
  stop("Package 'tinytex' is required. Install with install.packages('tinytex').", call. = FALSE)
}

pkg_root <- here::here()

resolve_input <- function(path) {
  if (grepl("^(?:[A-Za-z]:|/|\\\\\\\\)", path)) {
    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }

  candidates <- unique(c(
    file.path(getwd(), path),
    file.path(pkg_root, path)
  ))

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Could not locate manuscript input: ", path, call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
input <- resolve_input(if (length(args) >= 1L && nzchar(args[1])) {
  args[1]
} else {
  file.path("vignettes", "manuscript", "CausalMixGPD_JSS_article.Rnw")
})

manuscript_dir <- dirname(input)
input_name <- basename(input)
tex_name <- sub("\\.[Rr]nw$", ".tex", input_name)
pdf_name <- sub("\\.[Rr]nw$", ".pdf", input_name)

sync_local_asset <- function(name) {
  src <- file.path(manuscript_dir, "assets", name)
  dst <- file.path(manuscript_dir, name)
  if (!file.exists(src)) {
    return(invisible(FALSE))
  }
  if (!file.exists(dst) || file.info(src)$mtime > file.info(dst)$mtime) {
    file.copy(src, dst, overwrite = TRUE)
  }
  invisible(TRUE)
}

invisible(lapply(c("jss.cls", "jss.bst", "jsslogo.jpg"), sync_local_asset))

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(pkg_root, quiet = TRUE, export_all = FALSE)
}

oldwd <- setwd(manuscript_dir)
on.exit(setwd(oldwd), add = TRUE)

message("Knitting manuscript in: ", manuscript_dir)
invisible(knitr::knit(
  input = input_name,
  output = tex_name,
  quiet = TRUE,
  envir = globalenv()
))

message("Compiling PDF in: ", manuscript_dir)
invisible(tinytex::latexmk(tex_name, clean = FALSE))

pdf_path <- file.path(manuscript_dir, pdf_name)
if (!file.exists(pdf_path)) {
  stop("Expected output PDF was not created: ", pdf_path, call. = FALSE)
}

message("Rendered manuscript: ", normalizePath(pdf_path, winslash = "/", mustWork = TRUE))
