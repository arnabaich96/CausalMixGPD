## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 4.5,
  fig.align = "center"
)
library(ggplot2)
library(kableExtra)
set.seed(1)
FAST <- TRUE
mcmc <- if (FAST) {
  list(niter = 300, nburnin = 80, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 5000, nburnin = 1500, thin = 5, nchains = 2, seed = c(1, 2))
}

## ----results='hide'-----------------------------------------------------------
source("_load_pkg.R")

data("faithful", package = "datasets")
y <- faithful$eruptions
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)
fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)

## -----------------------------------------------------------------------------
print(fit)

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
if (interactive()) plot(fit, family = "traceplot")

