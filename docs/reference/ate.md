# Average treatment effects (ATE)

Computes treated-minus-control posterior means from a causal fit.

## Usage

``` r
ate(fit, newdata = NULL, interval = "credible", level = 0.95, nsim_mean = 200L)
```

## Arguments

- fit:

  A `"dpmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/DPmixGPD/reference/run_mcmc_causal.md).

- newdata:

  Optional data.frame or matrix of covariates for prediction.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- level:

  Numeric credible level for intervals (default 0.95 for 95% CI).

- nsim_mean:

  Number of posterior predictive draws to approximate the mean.

## Value

A list with elements `fit` (ATE), optional `lower`/`upper`, and the
treated/control prediction objects.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
ate(fit, newdata = X[1:5, ])
ate(fit, interval = "credible", level = 0.90)  # 90% CI
ate(fit, interval = "hpd")  # HPD intervals
ate(fit, interval = NULL)   # No intervals
} # }
```
