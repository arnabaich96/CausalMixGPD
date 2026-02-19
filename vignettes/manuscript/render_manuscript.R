#!/usr/bin/env Rscript
# Render manuscript with SyncTeX/concordance support
# Usage: Rscript render_manuscript.R

# Set working directory to manuscript folder
if (!grepl("manuscript$", getwd())) {
  if (file.exists("vignettes/manuscript")) {
    setwd("vignettes/manuscript")
  } else {
    stop("Please run from package root or manuscript directory", call. = FALSE)
  }
}

# Ensure jss.bst is in the manuscript directory
if (file.exists("assets/jss.bst") && !file.exists("jss.bst")) {
  message("Copying jss.bst from assets/ to manuscript directory...")
  file.copy("assets/jss.bst", "jss.bst", overwrite = TRUE)
}

# Render the Rmd with SyncTeX enabled
message("Rendering CausalMixGPD_JSS_article.Rmd with concordance support...")
rmarkdown::render(
  "CausalMixGPD_JSS_article.Rmd",
  output_format = rmarkdown::pdf_document(
    keep_tex = TRUE,
    latex_engine = "pdflatex",
    citation_package = "natbib",
    number_sections = TRUE,
    pandoc_args = c("--pdf-engine-opt=-synctex=1")
  ),
  clean = FALSE,
  envir = new.env(parent = globalenv())
)

# Check if synctex file was created
synctex_file <- "CausalMixGPD_JSS_article.synctex.gz"
if (file.exists(synctex_file)) {
  message("\n✓ SyncTeX file created: ", synctex_file)
  message("✓ PDF created: CausalMixGPD_JSS_article.pdf")
  message("✓ TeX source kept: CausalMixGPD_JSS_article.tex")
  message("\nConcordance is now enabled for forward/backward search between source and PDF.")
} else {
  warning("SyncTeX file was not created. Check the rendering output for errors.")
}
