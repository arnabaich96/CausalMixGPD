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
project_root <- here::here()

article_rnw <- file.path(manuscript_dir, "CausalMixGPD_JSS_article.Rnw")
article_tex <- file.path(manuscript_dir, "CausalMixGPD_JSS_article.tex")
article_synctex <- file.path(manuscript_dir, "CausalMixGPD_JSS_article.synctex.gz")
article_concord <- file.path(manuscript_dir, "CausalMixGPD_JSS_article-concordance.tex")

with_working_dir <- function(dir, expr) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dir)
  force(expr)
}

cleanup_root_artifacts <- function(root_dir) {
  stray_files <- file.path(
    root_dir,
    c(
      "CausalMixGPD_JSS_article.aux",
      "CausalMixGPD_JSS_article.bbl",
      "CausalMixGPD_JSS_article.blg",
      "CausalMixGPD_JSS_article.log",
      "CausalMixGPD_JSS_article.out",
      "CausalMixGPD_JSS_article.pdf",
      "CausalMixGPD_JSS_article.synctex.gz"
    )
  )

  existing_strays <- stray_files[file.exists(stray_files)]
  if (length(existing_strays)) {
    message("Removing stale manuscript artifacts from project root...")
    unlink(existing_strays, force = TRUE)
  }

  root_figure_dir <- file.path(root_dir, "figure")
  if (dir.exists(root_figure_dir)) {
    stray_figures <- list.files(
      root_figure_dir,
      pattern = "^.*-code-[0-9]+\\.(pdf|png)$",
      full.names = TRUE
    )
    if (length(stray_figures)) {
      message("Removing stale root-level manuscript figures...")
      unlink(stray_figures, force = TRUE)
    }
    if (!length(list.files(root_figure_dir, all.files = TRUE, no.. = TRUE))) {
      unlink(root_figure_dir, recursive = TRUE, force = TRUE)
    }
  }
}

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
old_concordance <- knitr::opts_knit$get("concordance")
on.exit(knitr::opts_knit$set(concordance = old_concordance), add = TRUE)
knitr::opts_knit$set(concordance = TRUE)
cleanup_root_artifacts(project_root)
with_working_dir(manuscript_dir, {
  knitr::knit(
    input = basename(article_rnw),
    output = basename(article_tex),
    quiet = FALSE,
    envir = new.env(parent = globalenv())
  )
})

message("Compiling CausalMixGPD_JSS_article.tex with pdflatex (SyncTeX enabled)...")
with_working_dir(manuscript_dir, {
  tinytex::latexmk(
    file = basename(article_tex),
    engine = "pdflatex",
    engine_args = "-synctex=1",
    bib_engine = "bibtex",
    clean = FALSE
  )
})

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
