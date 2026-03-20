# Effective sample size summaries for fitted models

`ess_summary()` reports effective sample size diagnostics for posterior
draws, optionally scaled by wall-clock time.

## Usage

``` r
ess_summary(
  fit,
  params = NULL,
  per_chain = TRUE,
  wall_time = NULL,
  robust = TRUE,
  ...
)
```

## Arguments

- fit:

  A `"mixgpd_fit"` or `"causalmixgpd_causal_fit"` object.

- params:

  Optional character vector of parameter names/patterns. If `NULL`, a
  fixed canonical set is auto-resolved.

- per_chain:

  Logical; if `TRUE`, include per-chain ESS rows.

- wall_time:

  Optional numeric total MCMC time in seconds. If `NULL`, uses
  `fit$timing$mcmc` when available.

- robust:

  Logical; if `TRUE`, skip missing parameters gracefully.

- ...:

  Unused.

## Value

Object of class `"mixgpd_ess_summary"` with elements `table`, `overall`,
and `meta`.

## Details

This is a convergence and efficiency diagnostic, not a model summary.
For causal fits the function evaluates each outcome arm separately and
tags the rows accordingly.

## See also

[`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md),
[`plot.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.mixgpd_fit.md),
[`params`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/params.md).
