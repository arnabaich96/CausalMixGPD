# Conditional quantile treatment effects (CQTE)

Computes treated-minus-control quantiles from a causal fit.

## Usage

``` r
cqte(
  fit,
  probs = c(0.1, 0.5, 0.9),
  newdata = NULL,
  interval = c("none", "credible")
)
```

## Arguments

- fit:

  A `"dpmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://example.com/DPmixGPD/reference/run_mcmc_causal.md).

- probs:

  Numeric vector of probabilities in (0, 1).

- newdata:

  Optional data.frame or matrix of covariates for prediction.

- interval:

  Credible interval type passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html).

## Value

A list with elements `fit` (CQTE), `grid` (probabilities), and the
treated/control prediction objects.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
cqte(fit, probs = c(0.5, 0.9), newdata = X[1:5, ])
} # }
```
