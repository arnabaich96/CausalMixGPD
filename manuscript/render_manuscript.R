args <- commandArgs(trailingOnly = TRUE)

input_rnw <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else "manuscript/CausalMixGPD_JSS_article.Rnw"
output_tex <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else "manuscript/CausalMixGPD_JSS_article.tex"

if (!file.exists(input_rnw)) {
  stop("Missing input file: ", input_rnw, call. = FALSE)
}

message("Knitting ", input_rnw, " -> ", output_tex)
knitr::knit(input_rnw, output = output_tex, quiet = TRUE)

pdf_out <- sub("\\.tex$", ".pdf", output_tex)

latexmk <- Sys.which("latexmk")
if (nzchar(latexmk)) {
  message("Running latexmk to build PDF: ", pdf_out)
  # Build from repo root so TeX paths in the manuscript remain stable.
  cmd <- sprintf('"%s" -pdf -interaction=nonstopmode -halt-on-error -outdir="%s" "%s"',
                 latexmk,
                 normalizePath(dirname(output_tex), winslash = "/", mustWork = TRUE),
                 normalizePath(output_tex, winslash = "/", mustWork = TRUE))
  status <- system(cmd)
  if (!identical(status, 0L)) {
    stop("latexmk failed with status ", status, call. = FALSE)
  }
} else {
  message("latexmk not found on PATH; skipping PDF build. (TeX output is at ", output_tex, ")")
}

