# Average treatment effects, marginal over the empirical covariate distribution

`ate()` computes the posterior predictive average treatment effect.

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

An object of class `"causalmixgpd_ate"` containing the marginal ATE
summary, optional intervals, and the arm-specific predictive objects
used in the aggregation. The returned object includes a top-level
`$fit_df` data frame for direct extraction.

## Details

The default mean-scale estimand is \$\$\mathrm{ATE} = E\\Y(1)\\ -
E\\Y(0)\\,\$\$ where the expectation is taken with respect to the
empirical training covariate distribution for conditional models.

When `type = "rmean"`, the function instead computes a restricted-mean
ATE using \\E\\\min(Y(a), c)\\\\ for each arm. For outcome kernels with
a finite analytical mean, the ordinary mean path is analytical within
each posterior draw; `rmean` remains simulation-based.

For unconditional causal models (`X = NULL`), the computation reduces to
a direct contrast of the unconditional treated and control predictive
laws.

## See also

[`att`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`ate_rmean`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate_rmean.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
ate(fit, interval = "credible", level = 0.90, nsim_mean = 100)
} # }
```
