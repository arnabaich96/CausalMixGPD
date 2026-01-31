# Validate bulk+tail glue for MixGPD predictive distribution

This diagnostic checks whether the implied predictive distribution
behaves like a valid distribution (monotone CDF in \\\[0,1\]\\,
nonnegative density, and sensible behavior around the threshold when a
GPD tail is enabled).

## Usage

``` r
check_glue_validity(
  fit,
  x = NULL,
  grid = NULL,
  n_draws = 50L,
  tol = 1e-08,
  check_continuity = TRUE,
  eps = 1e-06
)
```

## Arguments

- fit:

  A `mixgpd_fit` object.

- x:

  Optional design matrix for conditional models. If `NULL`, uses
  training `X`.

- grid:

  Numeric evaluation grid. If `NULL`, defaults to a grid based on
  training `y`.

- n_draws:

  Number of posterior draws to check (sampled without replacement when
  possible).

- tol:

  Numerical tolerance for monotonicity/range checks.

- check_continuity:

  Logical; if `TRUE` and GPD is enabled, checks continuity at the
  threshold.

- eps:

  Small offset used for threshold continuity check.

## Value

A list with per-check pass/fail flags and summaries of violations.

## Details

The check is performed draw-by-draw on a user-specified grid. It is
intended for development, debugging, and CI (not for routine large-scale
use).
