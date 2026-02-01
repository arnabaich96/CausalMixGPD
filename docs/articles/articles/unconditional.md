# Unconditional Models

## Overview

This vignette demonstrates fitting an unconditional DP mixture model
with optional GPD tail augmentation.

## Theory (brief)

Unconditional models treat observations as exchangeable draws from a
mixture distribution. The DP prior on the mixing measure $`G`$ yields
flexible density estimation: \$\$ y_i \\mid G \\sim \\int K(y_i;
\\theta)\\, dG(\\theta), \\quad G \\sim \\mathrm{DP}(\\alpha, G_0). \$\$
When \$\\mathrm{GPD} = \\mathrm{TRUE}\$, a GPD tail replaces the bulk
kernel beyond a threshold $`u`$ to stabilize tail behavior.

## Model Fitting

``` r

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
```

## Posterior Predictive Summaries

``` r
pred_mean <- predict(fit, type = "mean", cred.level = 0.90, interval = "credible")
pred_q95  <- predict(fit, type = "quantile", index = 0.95, cred.level = 0.90, interval = "credible")

pred_mean$fit
  id estimate lower upper
1  1     3.19  3.05  3.32
pred_q95$fit
  estimate index lower upper
1     4.81  0.95  4.72  4.93
```

## Note

For unconditional models, use only; and are not supported (they are for
conditional, covariate models).

## Diagnostic Plots

``` r

if (requireNamespace("ggmcmc", quietly = TRUE) && requireNamespace("coda", quietly = TRUE)) {
  if (interactive()) plot(fit)
} else {
  message("Plotting requires 'ggmcmc' and 'coda' packages.")
}
```
