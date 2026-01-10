## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/02-single-outcome-",
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

## ----single-example-----------------------------------------------------------
set.seed(42)
n <- 120
x <- rnorm(n)
y <- sim_bulk_tail(n, tail_prob = 0.18)
J <- 6
bundle <- build_nimble_bundle(
  y = y,
  X = data.frame(x = x),
  backend = "sb",
  kernel = "lognormal",
  GPD = TRUE,
  J = J,
  mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 2, seed = c(1, 2))
)
if (use_cached_fit) {
  fit <- fit_small
} else {
  fit <- run_mcmc_bundle_manual(bundle)
}
summary(fit)

## ----traceplot, fig.cap="Trace plot for tail parameters and weights."---------
plot(fit, family = "traceplot", params = c("alpha", "mean[1]"))

## ----prediction-api-----------------------------------------------------------
quant <- predict(fit, type = "quantile", p = c(.5, .9, .99))
quantile_df <- data.frame(
  tau = c(.5, .9, .99),
  value = as.numeric(quant$fit[1, ])
)
ggplot(quantile_df, aes(x = tau, y = value)) +
  geom_line() +
  geom_point() +
  labs(title = "Quantiles from the fitted model", x = "tau") +
  theme_minimal()

## ----bulk-vs-tail, fig.cap="Bulk density (blue) versus tail contribution (orange)."----
dense_grid <- seq(0, max(y) + 5, length.out = 120)
bulk_pred <- predict(fit, type = "density", y = dense_grid)
bulk_df <- data.frame(y = dense_grid, density = bulk_pred$fit[1, ])
tail_df <- data.frame(y = dense_grid, density = 1 - bulk_df$density / max(bulk_df$density))
ggplot(bulk_df, aes(x = y, y = density)) +
  geom_line(color = "#1f77b4") +
  geom_line(data = tail_df, aes(y = density), color = "#ff7f0e", linetype = "dashed") +
  labs(title = "Bulk vs Tail (scaled)", y = "Density estimate")

## ----posterior-summary-table--------------------------------------------------
tail_tab <- summary(fit)$table
keep <- tail_tab$parameter %in% c("threshold[1]", "tail_shape")
posterior_table <- tail_tab[keep, c("parameter", "mean", "sd", "q0.500")]
posterior_table <- rbind(posterior_table, data.frame(
  parameter = "predictive mean",
  mean = mean(predict(fit, type = "mean", nsim_mean = 100)$fit),
  sd = NA,
  q0.500 = NA
))
knitr::kable(posterior_table, digits = 3)

## ----session------------------------------------------------------------------
sessionInfo()

