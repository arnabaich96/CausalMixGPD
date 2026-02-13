# Quantile treatment effects (QTE), marginal over training covariates

Computes a marginal quantile treatment effect by averaging conditional
quantile effects over the training covariate rows.

## Usage

``` r
qte(
  fit,
  probs = c(0.1, 0.5, 0.9),
  newdata = NULL,
  y = NULL,
  interval = "credible",
  level = 0.95
)
```

## Arguments

- fit:

  A `"dpmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/run_mcmc_causal.md).

- probs:

  Numeric vector of probabilities in (0, 1) specifying the quantile
  levels of the outcome distribution to estimate treatment effects at.

- newdata:

  Deprecated placeholder for marginal estimands; must be `NULL`.

- y:

  Deprecated placeholder for marginal estimands; must be `NULL`.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

## Value

A list with elements `fit` (QTE), `grid` (probabilities), and aggregated
treated/control prediction objects.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
qte(fit, probs = c(0.5, 0.9))
} # }
```
