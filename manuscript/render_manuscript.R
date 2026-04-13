args <- commandArgs(trailingOnly = TRUE)

input_rnw <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else "manuscript/CausalMixGPD_JSS_article.Rnw"
output_tex <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else "manuscript/CausalMixGPD_JSS_article.tex"

if (!file.exists(input_rnw)) {
  stop("Missing input file: ", input_rnw, call. = FALSE)
}

message("Knitting ", input_rnw, " -> ", output_tex)
knitr::render_sweave()
knitr::knit(input_rnw, output = output_tex, quiet = TRUE)

pdf_out <- sub("\\.tex$", ".pdf", output_tex)

latexmk <- Sys.which("latexmk")
if (nzchar(latexmk)) {
  message("Running latexmk to build PDF: ", pdf_out)
  tex_dir <- normalizePath(dirname(output_tex), winslash = "/", mustWork = TRUE)
  tex_file <- basename(output_tex)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tex_dir)
  status <- system2(
    latexmk,
    args = c(
      "-pdf",
      "-synctex=1",
      "-interaction=nonstopmode",
      "-file-line-error",
      "-halt-on-error",
      tex_file
    )
  )
  if (!identical(status, 0L)) {
    stop("latexmk failed with status ", status, call. = FALSE)
  }
} else {
  message("latexmk not found on PATH; skipping PDF build. (TeX output is at ", output_tex, ")")
}

