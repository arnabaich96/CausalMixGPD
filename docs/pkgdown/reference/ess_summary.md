# Effective Sample Size Summary (ESS/sec)

Computes per-parameter effective sample size (ESS) and ESS-per-second
using posterior draws from a fitted object. For causal fits, summaries
are returned for both outcome arms and tagged by arm.

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
