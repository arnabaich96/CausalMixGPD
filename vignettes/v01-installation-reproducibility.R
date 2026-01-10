## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/01-installation-",
  fig.width = 6,
  warning = FALSE,
  message = FALSE
)
library(DPmixGPD)
library(nimble)
use_cached_fit <- TRUE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_small <- readRDS(.fit_path("fit_small.rds"))
library(ggplot2)

## ----reproducible-example-----------------------------------------------------
y <- sim_bulk_tail(n = 120, seed = 42)
if (use_cached_fit) {
  fit <- fit_small
} else {
  bundle <- build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "gamma",
    GPD = TRUE,
    J = 5,
    mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 2, seed = c(1, 2))
  )
  fit <- run_mcmc_bundle_manual(bundle)
}
print(fit)

## ----diag-density-------------------------------------------------------------
data.frame(y = y) |>
  ggplot(aes(x = y)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Input data density", x = "y", y = "Density")

## ----prediction-example-------------------------------------------------------
preds <- predict(fit, type = "quantile", p = c(0.5, 0.9, 0.95))
print(preds)

## ----session-info-------------------------------------------------------------
sessionInfo()

