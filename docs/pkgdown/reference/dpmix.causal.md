# Fit a causal two-arm Dirichlet process mixture without a GPD tail

`dpmix.causal()` fits a causal model with separate treated and control
outcome mixtures and, when requested, a propensity score block. It is
the bulk-only companion to
[`dpmgpd.causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.causal.md).

## Usage

``` r
dpmix.causal(
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

The resulting fit supports conditional outcome prediction \\F_a(y \mid
x, \mathcal{D})\\ for \\a \in \\0,1\\\\, followed by causal functionals
such as
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
and
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

## See also

[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`dpmgpd.causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.causal.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md).
