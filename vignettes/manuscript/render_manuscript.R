#!/usr/bin/env Rscript
# Render manuscript from Rnw with SyncTeX and concordance support
# Usage: Rscript render_manuscript.R

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Install with install.packages('here').", call. = FALSE)
}

manuscript_dir <- here::here("vignettes", "manuscript")
if (!dir.exists(manuscript_dir)) {
  stop("Could not locate 'vignettes/manuscript' from project root.", call. = FALSE)
}

article_rnw <- file.path(manuscript_dir, "CausalMixGPD_JSS_article.Rnw")
article_tex <- file.path(manuscript_dir, "CausalMixGPD_JSS_article.tex")
article_synctex <- file.path(manuscript_dir, "CausalMixGPD_JSS_article.synctex.gz")
article_concord <- file.path(manuscript_dir, "CausalMixGPD_JSS_article-concordance.tex")

if (file.exists(file.path(manuscript_dir, "assets", "jss.bst")) &&
    !file.exists(file.path(manuscript_dir, "jss.bst"))) {
  message("Copying jss.bst from assets/ to manuscript directory...")
  file.copy(file.path(manuscript_dir, "assets", "jss.bst"),
            file.path(manuscript_dir, "jss.bst"),
            overwrite = TRUE)
}
if (file.exists(file.path(manuscript_dir, "assets", "jsslogo.jpg")) &&
    !file.exists(file.path(manuscript_dir, "jsslogo.jpg"))) {
  message("Copying jsslogo.jpg from assets/ to manuscript directory...")
  file.copy(file.path(manuscript_dir, "assets", "jsslogo.jpg"),
            file.path(manuscript_dir, "jsslogo.jpg"),
            overwrite = TRUE)
}

message("Knitting CausalMixGPD_JSS_article.Rnw with concordance support...")
knitr::knit(
  input = article_rnw,
  output = article_tex,
  quiet = FALSE,
  envir = new.env(parent = globalenv()),
  concordance = TRUE
)

message("Compiling CausalMixGPD_JSS_article.tex with pdflatex (SyncTeX enabled)...")
tinytex::latexmk(
  file = article_tex,
  engine = "pdflatex",
  engine_args = "-synctex=1",
  bib_engine = "bibtex",
  clean = FALSE
)

if (file.exists(article_synctex)) {
  message("\n[OK] SyncTeX file created: ", article_synctex)
  message("[OK] PDF created: CausalMixGPD_JSS_article.pdf")
  message("[OK] TeX source kept: CausalMixGPD_JSS_article.tex")
} else {
  warning("SyncTeX file was not created. Check the rendering output for errors.")
}

if (file.exists(article_concord)) {
  message("[OK] Concordance file created: CausalMixGPD_JSS_article-concordance.tex")
} else {
  warning("Concordance file was not created. Check knitr output for errors.")
}
