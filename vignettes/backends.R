## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----backend-example----------------------------------------------------------
# bundle_sb <- build_nimble_bundle(y = y, kernel = "gamma", backend = "sb", J = 5)
# bundle_crp <- build_nimble_bundle(y = y, kernel = "gamma", backend = "crp", J = 5)

