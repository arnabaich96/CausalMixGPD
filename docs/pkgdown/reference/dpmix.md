# Fit a one-arm Dirichlet process mixture without a GPD tail

`dpmix()` is the one-step convenience wrapper for the bulk-only model.
It combines
[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md)
and
[`mcmc`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md)
for one-arm data.

## Usage

``` r
dpmix(
  y = NULL,
  X = NULL,
  treat = NULL,
  data = NULL,
  mcmc = list(),
  formula = NULL,
  ...
)
```

## Arguments

- y:

  Either a response vector or a bundle object.

- X:

  Optional design matrix/data.frame.

- treat:

  Optional binary treatment indicator. If supplied, this wrapper errors;
  use
  [`dpmix.causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.causal.md)
  for causal models.

- data:

  Optional data.frame used with `formula`.

- mcmc:

  Named list of run arguments passed to
  [`mcmc()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md)
  (including optional performance controls such as `parallel_chains`,
  `workers`, `timing`, and `z_update_every`).

- formula:

  Optional formula.

- ...:

  Additional build arguments passed to
  [`build_nimble_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md).

## Value

A fitted object of class `"mixgpd_fit"`.

## Details

The fitted model targets the posterior predictive bulk distribution
\$\$f(y \mid x) = \int f(y \mid x, \theta)\\d\Pi(\theta),\$\$ without
the spliced tail augmentation used by
[`dpmgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

Use this wrapper when the outcome support is adequately modeled by the
bulk kernel alone. If you need threshold exceedance modeling or
extreme-quantile extrapolation, use
[`dpmgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md)
instead.

## See also

[`build_nimble_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`dpmgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md),
[`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md).
