## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----mcmc-example-------------------------------------------------------------
# bundle <- build_nimble_bundle(
#   y = y,
#   kernel = "gamma",
#   backend = "crp",
#   GPD = TRUE,
#   J = 5,
#   mcmc = list(niter = 1000, nburnin = 250, nchains = 2)
# )
# fit <- run_mcmc_bundle_manual(bundle)
# plot(fit, family = c("traceplot", "running"))

