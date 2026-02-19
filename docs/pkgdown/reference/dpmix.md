# Fit DP mixture model without GPD tail

Fit DP mixture model without GPD tail

## Usage

``` r
dpmix(
  x = NULL,
  data = NULL,
  X = NULL,
  treat = NULL,
  formula = NULL,
  ...,
  mcmc = list()
)
```

## Arguments

- x:

  Either a response vector or a bundle object.

- data:

  Optional data.frame used with `formula`.

- X:

  Optional design matrix/data.frame.

- treat:

  Optional binary treatment indicator.

- formula:

  Optional formula.

- ...:

  Additional build arguments in build mode.

- mcmc:

  Named list of run arguments passed to
  [`mcmc()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md)
  (including optional performance controls such as `parallel_chains`,
  `parallel_arms`, `workers`, `timing`, and `z_update_every`).

## Value

A fitted object.
