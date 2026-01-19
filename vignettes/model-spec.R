## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
<<<<<<< HEAD
  comment = "#>",
  message = FALSE,
  warning = FALSE
)
set.seed(1)

## -----------------------------------------------------------------------------
library(DPmixGPD)

y <- abs(rnorm(40)) + 0.2
X <- data.frame(x = rnorm(40))

bundle <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  components = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 1, seed = 1)
)

bundle

## ----eval=FALSE---------------------------------------------------------------
# X <- data.frame(if = rnorm(40))
# # Fix:
# names(X)[names(X) == "if"] <- "x_if"
=======
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
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090

