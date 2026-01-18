## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 8,
  fig.height = 5,
  message = FALSE,
  warning = FALSE,
  eval = TRUE,
  cache = TRUE,
  cache.path = "../.cache/vignettes/"
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)

## -----------------------------------------------------------------------------
# Build
bundle <- build_nimble_bundle(
  y = rnorm(50),
  backend = "crp",
  kernel = "normal",
  GPD = FALSE,
  components = 5,
  mcmc = list(niter = 200, nburnin = 100, thin = 1, nchains = 1, seed = 1)
)

# Run
fit <- run_mcmc_bundle_manual(bundle)

# Summarize
print(fit)
summary(fit)
plot(fit)

## -----------------------------------------------------------------------------
kernel_support_table()

