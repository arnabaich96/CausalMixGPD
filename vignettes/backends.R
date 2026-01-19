## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
<<<<<<< HEAD
<<<<<<< HEAD
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 4.5
)
library(nimble)
set.seed(1)
FAST <- TRUE
mcmc <- if (FAST) {
  list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 9000, nburnin = 2500, thin = 5, nchains = 2, seed = c(1, 2))
}

## -----------------------------------------------------------------------------
library(DPmixGPD)

n <- 90
y <- abs(rnorm(n)) + 0.2
=======
=======
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
  comment = NA,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
library(DPmixGPD)

## ----backend-example----------------------------------------------------------
# bundle_sb <- build_nimble_bundle(y = y, kernel = "gamma", backend = "sb", J = 5)
# bundle_crp <- build_nimble_bundle(y = y, kernel = "gamma", backend = "crp", J = 5)
<<<<<<< HEAD
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090
=======
>>>>>>> 50289162bd36853addda01bb01ee507dfa332090

