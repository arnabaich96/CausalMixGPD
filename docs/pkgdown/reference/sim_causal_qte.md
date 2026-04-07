# Simulate causal quantile-treatment-effect data

Generate a treatment indicator, covariates, and a continuous outcome
with both location and tail heterogeneity. The resulting structure is
intended for examples involving
[`dpmix.causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.causal.md),
[`dpmgpd.causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.causal.md),
[`qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
and
[`cqte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

## Usage

``` r
sim_causal_qte(n = 300, seed = NULL)
```

## Arguments

- n:

  Integer sample size.

- seed:

  Optional random seed.

## Value

List with components `y`, `t`, and `X`; `A` is included as a
backward-compatible alias for `t`.

## Details

Treatment assignment is generated from a logistic propensity score \$\$
\Pr(T = 1 \mid X) = \operatorname{logit}^{-1}(\eta(X)), \$\$ and the
observed outcome combines baseline covariate effects, an average
treatment shift, and a covariate-dependent tail amplification for
treated units. This produces data where marginal and conditional
quantile effects differ across the outcome distribution.

The returned list can be converted directly into the arguments expected
by the causal fitting wrappers after minor formatting.

## See also

[`sim_bulk_tail()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_bulk_tail.md),
[`dpmgpd.causal()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.causal.md),
[`qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cqte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

Other simulation helpers:
[`sim_bulk_tail`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_bulk_tail.md)`()`,
[`sim_survival_tail`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_survival_tail.md)`()`
