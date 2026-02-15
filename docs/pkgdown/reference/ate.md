# Average treatment effects (ATE), marginal over training covariates

Computes a marginal average treatment effect by averaging conditional
treatment effects over the training covariate rows.

## Usage

``` r
ate(
  fit,
  newdata = NULL,
  y = NULL,
  type = c("mean", "rmean"),
  cutoff = NULL,
  interval = "credible",
  level = 0.95,
  nsim_mean = 200L
)
```

## Arguments

- fit:

  A `"causalmixgpd_causal_fit"` object from
  [`run_mcmc_causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md).

- newdata:

  Ignored for marginal estimands. If supplied, a warning is issued and
  training data are used.

- y:

  Ignored for marginal estimands. If supplied, a warning is issued and
  training data are used.

- type:

  Character; `"mean"` (default) for ordinary mean ATE or `"rmean"` for
  restricted-mean ATE.

- cutoff:

  Finite numeric cutoff for restricted mean; required for
  `type = "rmean"`, ignored otherwise.

- interval:

  Character or NULL; type of credible interval: `NULL` for no interval,
  `"credible"` for equal-tailed quantile intervals (default), or `"hpd"`
  for highest posterior density intervals.

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

- nsim_mean:

  Number of posterior predictive draws to approximate the mean.

## Value

A list with elements `fit`, optional `lower`/`upper`, and aggregated
treated/control prediction objects. `fit` is a single-value marginal
effect estimate, and intervals are computed from posterior draws.

## Details

For unconditional causal models (`X = NULL`), this is computed directly
from unconditional treated/control outcome predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
ate(fit, interval = "credible", level = 0.90, nsim_mean = 100)
} # }
```
