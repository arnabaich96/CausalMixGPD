## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 7,
  fig.height = 5,
  eval = TRUE,
  cache = TRUE,
  cache.path = "../.cache/vignettes/"
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)

## ----intro-quickstart---------------------------------------------------------
# Load packaged dataset
data("nc_pos200_k3")
y <- nc_pos200_k3$y

# Build unconditional model: bulk-only, SB backend
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "gamma",
  GPD = FALSE,
  components = 5,
  mcmc = list(niter = 200, nburnin = 10, nchains = 1)
)

# Print bundle
print(bundle)

## ----intro-deps---------------------------------------------------------------
packageVersion("DPmixGPD")
packageVersion("nimble")
packageVersion("ggplot2")

