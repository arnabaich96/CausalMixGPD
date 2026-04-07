# Fit a causal two-arm Dirichlet process mixture without a GPD tail

`dpmix.causal()` fits a causal model with separate treated and control
outcome mixtures and, when requested, a propensity score block. It is
the bulk-only companion to
[`dpmgpd.causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.causal.md).

## Usage

``` r
dpmix.causal(
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

  Either a response vector or a causal bundle object.

- X:

  Optional design matrix/data.frame.

- treat:

  Binary treatment indicator.

- data:

  Optional data.frame used with `formula`.

- mcmc:

  Named list of run arguments passed to
  [`mcmc()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md)
  (including optional performance controls such as `parallel_arms`,
  `workers`, `timing`, and `z_update_every`).

- formula:

  Optional formula.

- ...:

  Additional build arguments passed to
  [`build_causal_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_causal_bundle.md).

## Value

A fitted object of class `"causalmixgpd_causal_fit"`.

## Details

The resulting fit supports conditional outcome prediction \\F_a(y \mid
x)\\ for \\a \in \\0,1\\\\, followed by causal functionals such as
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
and
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

## See also

[`build_causal_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_causal_bundle.md),
[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`dpmgpd.causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.causal.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md).
