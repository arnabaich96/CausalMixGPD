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
library(DPmixGPD)
# Ensure kernel registry is available (may not be initialized in pkgdown environment)
if (!exists("kernel_registry", envir = asNamespace("DPmixGPD"), inherits = FALSE)) {
  init_kernel_registry()
}
library(ggplot2)
library(kableExtra)
set.seed(1)
FAST <- TRUE
mcmc <- if (FAST) {
  list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 4000, nburnin = 1000, thin = 5, nchains = 2, seed = c(1, 2))
}

## ----minimal-run, results='hide'----------------------------------------------
library(DPmixGPD)

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

## For unconditional models use predict() only; fitted() and residuals() are for conditional models.

## ----predict------------------------------------------------------------------
pred_mean <- predict(fit, type = "mean", cred.level = 0.90, interval = "credible")
pred_q90  <- predict(fit, type = "quantile", index = 0.90, cred.level = 0.90, interval = "credible")

pred_mean$fit
pred_q90$fit

## ----plot-fit-----------------------------------------------------------------
if (requireNamespace("ggmcmc", quietly = TRUE) && requireNamespace("coda", quietly = TRUE)) {
  if (interactive()) plot(fit)
} else {
  message("Plotting requires 'ggmcmc' and 'coda' packages.")
}

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

