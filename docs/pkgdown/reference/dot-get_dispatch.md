# Resolve kernel dispatch functions Dispatch returns vector-aware d/p/q and n-aware r via wrappers; do not mutate namespace.

Resolve kernel dispatch functions Dispatch returns vector-aware d/p/q
and n-aware r via wrappers; do not mutate namespace.

## Usage

``` r
.get_dispatch(spec_or_fit, backend_override = NULL, gpd_override = NULL)
```

## Arguments

- spec_or_fit:

  mixgpd_fit or spec list

## Value

List with d/p/q/r/mean/mean_trunc functions and bulk_params.

## Details

This is the prediction-oriented companion to
[`.get_dispatch_scalar()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dot-get_dispatch_scalar.md).
It starts from the same kernel dispatch lookup, then wraps the scalar
functions so they can accept vector inputs and the package's preferred
argument naming conventions in ordinary R evaluation.
