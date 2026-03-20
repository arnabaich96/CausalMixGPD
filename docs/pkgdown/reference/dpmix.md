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

  Optional binary treatment indicator. If supplied, this wrapper errors;
  use
  [`dpmix.causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.causal.md)
  for causal models.

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

A fitted object of class `"mixgpd_fit"`.

## Details

The fitted model targets the posterior predictive bulk distribution
\$\$f(y \mid x, \mathcal{D}) = \int f(y \mid x, \theta)\\d\Pi(\theta
\mid \mathcal{D}),\$\$ without the spliced tail augmentation used by
[`dpmgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

Use this wrapper when the outcome support is adequately modeled by the
bulk kernel alone. If you need threshold exceedance modeling or
extreme-quantile extrapolation, use
[`dpmgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md)
instead.

## See also

[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`dpmgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md),
[`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md).
