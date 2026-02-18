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
knitr::knit('CausalMixGPD_JSS_article.Rnw', output = 'CausalMixGPD_JSS_article.tex')
tinytex::latexmk('CausalMixGPD_JSS_article.tex', engine = 'pdflatex', engine_args = '-synctex=1')

