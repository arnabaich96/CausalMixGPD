## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----causal-outline-----------------------------------------------------------
# # Pseudocode only (not executed)
# fit0 <- build_nimble_bundle(y = y0, X = X0, kernel = "gamma", GPD = TRUE)
# fit1 <- build_nimble_bundle(y = y1, X = X1, kernel = "gamma", GPD = TRUE)
# # run MCMC for each, then compare predictive distributions

