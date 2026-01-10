## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/07-survival-",
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

## ----survival-example---------------------------------------------------------
data <- sim_survival_tail(n = 220, seed = 220)
y <- data$time
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "gamma",
  GPD = TRUE,
  J = 5,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 2, seed = c(1, 2))
)
if (use_cached_fit) {
  fit <- fit_small
} else {
  fit <- run_mcmc_bundle_manual(bundle)
}

## ----survival-density---------------------------------------------------------
data.frame(time = y, status = factor(data$status, labels = c("censored", "event"))) |>
  ggplot(aes(x = time, fill = status)) +
  geom_density(alpha = 0.4) +
  labs(title = "Observed survival times (censored vs event)", x = "Time", fill = "Status")

## ----survival-pred------------------------------------------------------------
tau <- seq(0.01, 0.99, length.out = 40)
quantiles <- predict(fit, type = "quantile", p = tau)
surv_curve <- data.frame(time = as.numeric(quantiles$fit), survival = 1 - tau)
surv_curve

## ----session-info-------------------------------------------------------------
sessionInfo()

