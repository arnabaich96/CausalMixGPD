# Average treatment effects standardized to treated covariates

`att()` computes the average treatment effect on the treated.

## Usage

``` r
att(
  fit,
  newdata = NULL,
  y = NULL,
  type = c("mean", "rmean"),
  cutoff = NULL,
  interval = "credible",
  level = 0.95,
  nsim_mean = 200L,
  show_progress = TRUE
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

  Character; type of mean treatment effect:

  - `"mean"` (default): ordinary mean ATE

  - `"rmean"`: restricted-mean ATE (requires `cutoff`)

- cutoff:

  Finite numeric cutoff for restricted mean; required for
  `type = "rmean"`, ignored otherwise.

- interval:

  Character or NULL; type of credible interval:

  - `NULL`: no interval

  - `"credible"` (default): equal-tailed quantile intervals

  - `"hpd"`: highest posterior density intervals

- level:

  Numeric credible level for intervals (default 0.95 for 95 percent CI).

- nsim_mean:

  Number of posterior predictive draws used by simulation-based mean
  targets. Ignored for analytical ordinary means.

- show_progress:

  Logical; if TRUE, print step messages and render progress where
  supported.

## Value

An object of class `"causalmixgpd_ate"` containing the ATT summary,
optional intervals, and the arm-specific predictive objects used in the
aggregation. The returned object includes a top-level `$fit_df` data
frame for direct extraction.

## Details

The estimand is \$\$\mathrm{ATT} = E\\Y(1) - Y(0) \mid A = 1\\,\$\$
approximated by marginalizing over the empirical covariate distribution
of treated units.

## See also

[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qtt`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qtt.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
att(fit, interval = "credible", nsim_mean = 100)
} # }
```
