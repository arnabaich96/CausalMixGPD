#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
build_pdf <- !"--no-pdf" %in% args

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arg)) {
  stop("Unable to determine script path from commandArgs().")
}

script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
script_dir <- dirname(script_path)
project_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)

rnw_path <- file.path(script_dir, "CausalMixGPD_JSS_article.Rnw")
tex_path <- file.path(script_dir, "CausalMixGPD_JSS_article.tex")
pdf_path <- file.path(script_dir, "CausalMixGPD_JSS_article.pdf")

if (!file.exists(rnw_path)) {
  stop("Rnw file not found: ", rnw_path)
}

if (!requireNamespace("knitr", quietly = TRUE)) {
  stop("Package 'knitr' is required but not installed in this R environment.")
}

old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

message("[render_manuscript] Knitting Rnw to TeX...")
knitr::knit(input = rnw_path, output = tex_path, quiet = FALSE)
message("[render_manuscript] TeX generated: ", tex_path)

if (build_pdf) {
  message("[render_manuscript] Compiling TeX to PDF with tools::texi2pdf()...")
  tools::texi2pdf(file = tex_path, clean = TRUE, quiet = TRUE)

  if (!file.exists(pdf_path)) {
    stop("PDF compilation did not produce: ", pdf_path)
  }

  message("[render_manuscript] PDF generated: ", pdf_path)
} else {
  message("[render_manuscript] Skipping PDF compilation (--no-pdf).")
}