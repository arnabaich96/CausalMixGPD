## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/09-troubleshooting-",
  warning = FALSE,
  message = FALSE,
  fig.width = 6
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

## ----init-note----------------------------------------------------------------
bundle <- build_nimble_bundle(y = abs(rnorm(40)) + 0.1, backend = "sb", kernel = "normal", J = 5)
if (use_cached_fit) {
  fit <- fit_small
} else {
  fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
}
fit$bundle$code  # ensures model objects exist

## ----dimension-check----------------------------------------------------------
bad_X <- matrix(1, nrow = 5, ncol = 1)
stopifnot(ncol(bad_X) == ncol(bundle$dimensions$X %||% matrix()))

## ----weight-hist, fig.cap="Posterior weights that survived truncation."-------
ws <- draw_w <- DPmixGPD:::.extract_draws_matrix(fit, drop_v = TRUE)
weight_cols <- grep("^w\\[[0-9]+\\]$", colnames(draw_w), value = TRUE)
weight_df <- data.frame(weight = c(draw_w[, weight_cols]))
ggplot(weight_df, aes(x = weight)) +
  geom_histogram(bins = 25, fill = "#1f77b4", color = "white") +
  labs(title = "Posterior weight distribution (some components may collapse)")

## ----decision-chart-----------------------------------------------------------
symptoms <- data.frame(
  symptom = c("Dimension mismatch", "Not fully initialized", "Tail shape blows up", "Slow mixing"),
  cause = c("newdata matrix columns differ", "latent nodes not set before compile", "too few tail exceedances", "thin/nchains too small"),
  fix = c("match columns/newdata", "provide explicit inits", "increase components/J or seed", "increase nburnin/thin or reduce autocorr")
)
knitr::kable(symptoms)

## ----predictive-snapshot------------------------------------------------------
quant <- predict(fit, type = "quantile", p = c(.5, .9, .99))
quant

## ----session------------------------------------------------------------------
sessionInfo()

