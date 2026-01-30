# Fitted values and residuals for a MixGPD fit

Computes fitted values and residuals on the original training data for
**conditional (covariate) models only**. Returns a data frame with point
estimates, credible intervals, and residuals. Not supported for unconditional
models (no covariates); use `predict()` for predictions in that case.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
fitted(
  object,
  type = c("location", "mean", "median", "quantile"),
  p = 0.5,
  level = 0.95,
  interval = "credible",
  seed = 1,
  ...
)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- type:

  Which fitted location to return: mean, median, quantile, or both
  (`"location"`).

- p:

  Quantile level used when `type = "quantile"`.

- level:

  Credible level for confidence intervals (default 0.95 for 95 percent
  credible intervals).

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- seed:

  Random seed used for deterministic fitted values.

- ...:

  Unused.

## Value

A data frame with columns: `fit` (point estimates), `lower` (lower
credible bound), `upper` (upper credible bound), and `residuals` (y -
fit).

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
fitted(fit)
fitted(fit, level = 0.90)
fitted(fit, interval = "hpd")  # HPD intervals
fitted(fit, interval = NULL)   # No intervals
} # }
```
