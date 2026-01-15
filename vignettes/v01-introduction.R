## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  fig.width = 7,
  fig.height = 5,
  eval = FALSE
)
library(DPmixGPD)
if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(quiet = TRUE)

## ----intro-quickstart---------------------------------------------------------
# # Generate data
# set.seed(123)
# y <- abs(rnorm(50)) + 0.1
# 
# # Build unconditional model: bulk-only, SB backend
# bundle <- build_nimble_bundle(
#   y = y,
#   backend = "sb",
#   kernel = "gamma",
#   GPD = FALSE,
#   components = 6,
#   mcmc = list(niter = 200, nburnin = 50, nchains = 1)
# )
# 
# # Print bundle
# print(bundle)

## ----intro-deps---------------------------------------------------------------
# packageVersion("DPmixGPD")
# packageVersion("nimble")
# packageVersion("ggplot2")

