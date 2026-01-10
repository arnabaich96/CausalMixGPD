## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/backends-",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library(DPmixGPD)
library(nimble)
use_cached_fit <- TRUE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_small <- readRDS(.fit_path("fit_small.rds"))
fit_small_crp <- readRDS(.fit_path("fit_small_crp.rds"))
set.seed(2)
N <- 60
y <- abs(rnorm(N)) + 0.1

## -----------------------------------------------------------------------------
if (use_cached_fit) {
  fit_sb <- fit_small
} else {
  fit_sb <- run_mcmc_bundle_manual(build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    J = 8,
    mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
  ), show_progress = FALSE)
}

summary(fit_sb)

## -----------------------------------------------------------------------------
if (use_cached_fit) {
  fit_crp <- fit_small_crp
} else {
  fit_crp <- run_mcmc_bundle_manual(build_nimble_bundle(
    y = y,
    backend = "crp",
    kernel = "normal",
    J = 8,
    mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
  ), show_progress = FALSE)
}

summary(fit_crp)

## -----------------------------------------------------------------------------
q_sb <- predict(fit_sb, type = "quantile", p = c(0.5, 0.9))$fit
q_crp <- predict(fit_crp, type = "quantile", p = c(0.5, 0.9))$fit

rbind(sb = q_sb, crp = q_crp)

