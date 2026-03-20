# Fit a causal two-arm Dirichlet process mixture with a spliced GPD tail

`dpmgpd.causal()` is the highest-level causal fitting wrapper. It builds
or accepts a causal bundle, runs posterior sampling for the treated and
control arms, and returns a single causal fit ready for prediction and
effect estimation.

## Usage

``` r
dpmgpd.causal(
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

  Either a response vector or a causal bundle object.

- data:

  Optional data.frame used with `formula`.

- X:

  Optional design matrix/data.frame.

- treat:

  Binary treatment indicator.

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

A fitted object of class `"causalmixgpd_causal_fit"`.

## Details

The arm-specific predictive distributions \\F_1(y \mid x, \mathcal{D})\\
and \\F_0(y \mid x, \mathcal{D})\\ inherit the spliced bulk-tail
structure. Downstream causal estimands are computed as functionals of
these two predictive laws, for example \$\$\mathrm{QTE}(\tau) =
Q_1(\tau) - Q_0(\tau), \qquad \mathrm{ATE} = E(Y_1) - E(Y_0).\$\$

## See also

[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`dpmix.causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.causal.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).
