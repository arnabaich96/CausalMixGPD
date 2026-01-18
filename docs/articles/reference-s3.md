# S3 Methods Reference

## Core S3 methods

| Method | Object | Purpose | Output |
|----|----|----|----|
| [`print()`](https://rdrr.io/r/base/print.html) | `dpmixgpd_bundle` | quick bundle summary | concise configuration |
| [`summary()`](https://rdrr.io/r/base/summary.html) | `dpmixgpd_bundle` | detailed bundle summary | lists code/data/monitors |
| [`print()`](https://rdrr.io/r/base/print.html) | `mixgpd_fit` | fit summary | kernel/backends/mcmc info |
| [`summary()`](https://rdrr.io/r/base/summary.html) | `mixgpd_fit` | posterior summary | parameter tables |
| [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | `mixgpd_fit` | diagnostics | trace, running mean, autocorrelation |
| [`predict()`](https://rdrr.io/r/stats/predict.html) | `mixgpd_fit` | posterior predictive | density, quantile, mean, survival |

## Parameter naming conventions

Common parameter names in outputs:

- `w` (mixture weights)
- `alpha` (concentration)
- kernel parameters (e.g., `mean`, `sd`, `shape`, `scale`)
- link coefficients: `beta_<param>`
- GPD links: `beta_threshold`, `beta_tail_scale`

## Typical usage patterns

``` r
fit <- run_mcmc_bundle_manual(bundle)
print(fit)
summary(fit)
plot(fit, family = c("traceplot", "running"))
q_pred <- predict(fit, type = "quantile", index = c(0.1, 0.5, 0.9))
plot(q_pred)
```

## Notes

- [`predict()`](https://rdrr.io/r/stats/predict.html) returns a
  structured object with `fit` and interval summaries.
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) accepts
  families such as `traceplot`, `running`, and `autocorrelation`.
- Use
  [`params()`](https://arnabaich96.github.io/DPmixGPD/reference/params.md)
  for convenient access to posterior draws in original scales.
