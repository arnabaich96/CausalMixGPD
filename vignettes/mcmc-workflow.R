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
  list(niter = 400, nburnin = 100, thin = 2, nchains = 1, seed = 1)
} else {
  list(niter = 6000, nburnin = 2000, thin = 5, nchains = 2, seed = c(1, 2))
}

## ----results='hide'-----------------------------------------------------------
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

## -----------------------------------------------------------------------------
print(fit)
summary(fit)

## -----------------------------------------------------------------------------
if (requireNamespace("ggmcmc", quietly = TRUE) && requireNamespace("coda", quietly = TRUE)) {
  smp <- fit$mcmc$samples
  params <- if (!is.null(smp)) {
    cn <- colnames(as.matrix(smp))
    cn[1:min(6, length(cn))]
  } else {
    NULL
  }
  if (interactive()) plot(fit, family = "geweke", params = params)
} else {
  message("Plotting requires 'ggmcmc' and 'coda' packages.")
}

## -----------------------------------------------------------------------------
if (!is.null(fit$mcmc$samples)) {
  s <- fit$mcmc$samples
  if (requireNamespace("coda", quietly = TRUE)) {
    mat <- as.matrix(s)
    dim(mat)
    colnames(mat)[1:min(20, ncol(mat))]
  } else {
    message("Sample extraction requires 'coda' package.")
  }
}

## ----eval=FALSE---------------------------------------------------------------
# # Rebuild with modified MCMC settings
# # bundle2 <- update_mcmc(bundle, niter = 8000, nburnin = 2000)
# # fit2 <- run_mcmc_bundle_manual(bundle2)

## ----sessioninfo, include=FALSE-----------------------------------------------
sessionInfo()

