## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE,
  cache = TRUE,
  cache.path = "../.cache/vignettes/"
)
library(DPmixGPD)

## -----------------------------------------------------------------------------
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "amoroso",
  GPD     = TRUE,
  components = 6
)

