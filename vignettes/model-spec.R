## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----bundle-example-----------------------------------------------------------
# bundle <- build_nimble_bundle(
#   y = y,
#   X = X,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = TRUE,
#   J = 5,
#   mcmc = list(niter = 200, nburnin = 50, nchains = 1)
# )

