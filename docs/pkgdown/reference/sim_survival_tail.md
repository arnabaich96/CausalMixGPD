# Simulate censored survival-style tail data

Generate event times, censoring times, an event indicator, and
covariates for examples where right tail behavior and positive support
matter.

## Usage

``` r
sim_survival_tail(n = 250, seed = NULL)
```

## Arguments

- n:

  Integer sample size.

- seed:

  Optional random seed.

## Value

Data frame containing observed time `time`, event indicator `status`,
and covariates.

## Details

Event times are sampled from an exponential model with
covariate-dependent mean, then censored by an independent uniform
censoring time. The observed time is \$\$ \tilde{T} = \min(T, C), \qquad
\Delta = I(T \le C). \$\$

This helper is mainly for experimentation and stress-testing
positive-support kernels; it does not implement a dedicated survival
model from the package API.

## See also

[`sim_bulk_tail()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_bulk_tail.md),
[`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

Other simulation helpers:
[`sim_bulk_tail`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_bulk_tail.md)`()`,
[`sim_causal_qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_causal_qte.md)`()`
