## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/03-backends-",
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
fit_small_crp <- readRDS(.fit_path("fit_small_crp.rds"))
library(ggplot2)

## ----backend-example----------------------------------------------------------
y <- sim_bulk_tail(n = 150, tail_prob = 0.08, seed = 101)
common_args <- list(
  y = y,
  kernel = "lognormal",
  GPD = TRUE,
  J = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 2, seed = c(1, 2))
)

if (use_cached_fit) {
  fit_sb <- fit_small
  fit_crp <- fit_small_crp
  sb_time <- c(elapsed = NA_real_)
  crp_time <- c(elapsed = NA_real_)
} else {
  sb_time <- system.time({
    bundle_sb <- do.call(build_nimble_bundle, c(list(backend = "sb"), common_args))
    fit_sb <- run_mcmc_bundle_manual(bundle_sb)
  })
  crp_time <- system.time({
    bundle_crp <- do.call(build_nimble_bundle, c(list(backend = "crp"), common_args))
    fit_crp <- run_mcmc_bundle_manual(bundle_crp)
  })
}

system_time <- data.frame(
  backend = c("SB", "CRP"),
  elapsed = c(sb_time[["elapsed"]], crp_time[["elapsed"]])
)
system_time

## ----backend-plot-------------------------------------------------------------
q_sb <- predict(fit_sb, type = "quantile", p = c(0.5, 0.9))
q_crp <- predict(fit_crp, type = "quantile", p = c(0.5, 0.9))

data.frame(
  backend = rep(c("SB", "CRP"), each = 2),
  prob = rep(c(0.5, 0.9), 2),
  quantile = c(as.numeric(q_sb$fit), as.numeric(q_crp$fit))
) |>
  ggplot(aes(x = prob, y = quantile, color = backend)) +
  geom_point() +
  geom_line() +
  labs(title = "Quantiles by backend", y = "Quantile", x = "Probability")

## ----backend-pred-------------------------------------------------------------
quantiles_sb <- predict(fit_sb, type = "quantile", p = c(.95, .99))
quantiles_crp <- predict(fit_crp, type = "quantile", p = c(.95, .99))
list(sb = quantiles_sb, crp = quantiles_crp)

## ----session-info-------------------------------------------------------------
sessionInfo()

