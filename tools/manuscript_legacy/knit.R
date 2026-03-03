# === Manuscript knitting script with model caching ===
# 
# This script knits the CausalMixGPD_JSS_article.Rnw manuscript.
# 
# Model caching:
#   - First knit: Fits all models and caches them in manifest/cache/
#   - Subsequent knits: Loads cached models (much faster!)
#   - To regenerate cache: Edit knit.R and set knitr_params$use_cache = FALSE
#   - To clear cache: unlink("cache", recursive = TRUE)
#

knitr::opts_knit[['set']](concordance = TRUE)
if (!grepl("vignettes[/\\\\]manuscript$", getwd())) {
  if (file.exists("vignettes/manuscript")) {
    setwd("vignettes/manuscript")
  } else {
    stop("Please run from package root or manuscript directory.", call. = FALSE)
  }
}

utils::Sweave('CausalMixGPD_JSS_article.Rnw')
tools::texi2pdf('CausalMixGPD_JSS_article.tex', clean = FALSE)

