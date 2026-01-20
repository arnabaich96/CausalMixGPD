# Average treatment effects (ATE)

Computes treated-minus-control posterior means from a causal fit.

## Usage

``` r
ate(
  fit,
  newdata = NULL,
  interval = c("none", "credible"),
  nsim_mean = 200L,
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- fit:

  A `"dpmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/DPmixGPD/reference/run_mcmc_causal.md).

- newdata:

  Optional data.frame or matrix of covariates for prediction.

- interval:

  Credible interval type passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html).

- nsim_mean:

  Number of posterior predictive draws to approximate the mean.

- probs:

  Quantiles for credible intervals when `interval="credible"`.

## Value

A list with elements `fit` (ATE), optional `lower`/`upper`, and the
treated/control prediction objects.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
ate(fit, newdata = X[1:5, ])
} # }
```
