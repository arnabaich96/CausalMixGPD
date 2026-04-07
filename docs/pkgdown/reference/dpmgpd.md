# Fit a one-arm Dirichlet process mixture with a spliced GPD tail

`dpmgpd()` is the one-step convenience wrapper for the spliced bulk-tail
model. It combines
[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md)
and
[`mcmc`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md)
for one-arm data.

## Usage

``` r
dpmgpd(
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
  [`dpmgpd.causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.causal.md)
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

This wrapper targets the posterior predictive distribution obtained by
combining a flexible bulk DPM with a generalized Pareto exceedance model
above the threshold \\u(x)\\. In the tail region the predictive density
is proportional to \$\$\\1 - p_u(x)\\ f\_{\mathrm{GPD}}(y \mid x),
\qquad y \> u(x),\$\$ where \\p_u(x)\\ is the posterior bulk mass below
the threshold.

Use this wrapper when upper-tail behavior matters for inference,
prediction, or extrapolation of extreme quantiles and survival
probabilities.

## See also

[`build_nimble_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`dpmix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md),
[`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md).
