#!/usr/bin/env Rscript
devtools::load_all(quiet = TRUE)
rmarkdown::render(
  'vignettes/v05-unconditional-DPmix-SB.Rmd',
  output_file = 'test_v05_exec.html',
  quiet = FALSE
)
