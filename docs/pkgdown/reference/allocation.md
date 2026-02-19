# Extract posterior cluster allocation

Computes posterior cluster allocations from a fitted Dirichlet process
mixture model using the Chinese Restaurant Process (CRP) or spliced
backend. The method computes the posterior similarity matrix (PSM),
identifies a representative partition using Dahl's method, and
optionally predicts cluster memberships for new data.

## Usage

``` r
allocation(object, ...)
```

## Arguments

- object:

  A fitted model object.

- ...:

  Additional arguments passed to methods.

## Value

An object of class `"mixgpd_allocation"`.

## Details

This function is only available for models fit with `backend = "crp"` or
`backend = "spliced"` and `GPD = TRUE`. Stick-breaking (SB) models and
bulk-only models are not supported.

## See also

[`print.mixgpd_allocation`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.mixgpd_allocation.md),
[`summary.mixgpd_allocation`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_allocation.md),
[`plot.mixgpd_allocation`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.mixgpd_allocation.md)
