## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/10-developer-",
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

## ----kernel-template, eval=FALSE----------------------------------------------
# # Dummy skeleton (copy into R/0-base-kernels.R and register)
# nimbleFunction(
#   run = function(y = double(1), mu = double(0), sigma = double(0)) {
#     returnType(double(0))
#     dnorm(y, mu, sigma, log = 1)
#   }
# )

## ----register-template, eval=FALSE--------------------------------------------
# init_kernel_registry("custom", list(
#   density = "dCustomMix",
#   quantile = "qCustomMix",
#   rng = "rCustomMix",
#   support = "(0, Inf)",
#   params = c("mu", "sigma")
# ))

## ----developer-pred-----------------------------------------------------------
y <- sim_bulk_tail(n = 80, seed = 123)
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "lognormal",
  GPD = FALSE,
  J = 4,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 2, seed = c(1, 2))
)
if (use_cached_fit) {
  fit <- fit_small
} else {
  fit <- run_mcmc_bundle_manual(bundle)
}
predict(fit, type = "quantile", p = c(0.5, 0.9))

## ----developer-plot-----------------------------------------------------------
data.frame(y = y) |>
  ggplot(aes(x = y)) +
  geom_freqpoly(color = "steelblue") +
  labs(title = "Developer-mode data distribution", x = "y", y = "Count")

## ----session-info-------------------------------------------------------------
sessionInfo()

