# Quantile treatment effects (QTE), marginal over training covariates

Computes a marginal quantile treatment effect as a contrast of
arm-specific marginal quantiles: \\Q_1^{m}(\tau) - Q_0^{m}(\tau)\\.

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

  A `"causalmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md).

- probs:

  Numeric vector of probabilities in (0, 1) specifying the quantile
  levels of the outcome distribution to estimate treatment effects at.

- newdata:

  Ignored for marginal estimands. If supplied, a warning is issued and
  training data are used.

- y:

  Ignored for marginal estimands. If supplied, a warning is issued and
  training data are used.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

## Value

A list with elements `fit`, `grid` (probabilities), and aggregated
treated/control prediction objects. `fit` is a data frame with columns
`index`, `estimate`, `lower`, `upper` and one row per requested quantile
index.

## Details

For conditional models, each posterior draw induces arm-specific
marginal predictive distributions by averaging over the empirical
covariate distribution, then quantiles are taken from those marginal
draws.

For unconditional causal models (`X = NULL`), this is computed directly
from unconditional treated/control outcome predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
qte(fit, probs = c(0.5, 0.9))
} # }
```
