# Quantile treatment effects (QTE)

Computes treated-minus-control quantiles from a causal fit.

## Usage

``` r
qte(
  fit,
  probs = c(0.1, 0.5, 0.9),
  newdata = NULL,
  interval = "credible",
  level = 0.95
)
```

## Arguments

- fit:

  A `"dpmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/DPmixGPD/reference/run_mcmc_causal.md).

- probs:

  Numeric vector of probabilities in (0, 1) specifying the quantile
  levels of the outcome distribution to estimate treatment effects at.

- newdata:

  Optional data.frame or matrix of covariates for prediction.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- level:

  Numeric credible level for intervals (default 0.95 for 95% CI).

## Value

A list with elements `fit` (QTE), `grid` (probabilities), and the
treated/control prediction objects.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
qte(fit, probs = c(0.5, 0.9), newdata = X[1:5, ])
qte(fit, probs = c(0.5, 0.9), interval = "credible", level = 0.90)  # 90% CI
qte(fit, probs = c(0.5, 0.9), interval = "hpd")  # HPD intervals
qte(fit, probs = c(0.5, 0.9), interval = NULL)   # No intervals
} # }
```
