# Fitted values on the training design

`fitted.mixgpd_fit()` is a thin training-data wrapper around
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md)
for conditional models.

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

  A fitted object of class `"mixgpd_fit"` (must have covariates).

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

A data frame with columns for fitted values, optional intervals, and
residuals computed on the training sample.

## Details

The method returns posterior predictive fitted values on the observed
design matrix. It is available only when the fitted model stored
covariates.

Use `type = "location"` to retrieve both posterior predictive means and
medians side by side. For unconditional models, use
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md)
directly.

## See also

[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md),
[`residuals.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/residuals.mixgpd_fit.md),
[`plot.mixgpd_fitted`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.mixgpd_fitted.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Conditional model (with covariates X)
y <- abs(stats::rnorm(50)) + 0.1
X <- data.frame(x1 = stats::rnorm(50), x2 = stats::runif(50))
bundle <- build_nimble_bundle(y = y, X = X, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
fitted(fit)
fitted(fit, level = 0.90)
fitted(fit, interval = "hpd")  # HPD intervals
fitted(fit, interval = NULL)   # No intervals
} # }
```
