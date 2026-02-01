# Resolve kernel dispatch functions Dispatch returns vector-aware d/p/q and n-aware r via wrappers; do not mutate namespace.

Resolve kernel dispatch functions Dispatch returns vector-aware d/p/q
and n-aware r via wrappers; do not mutate namespace.

## Usage

``` r
.get_dispatch(spec_or_fit, backend_override = NULL)
```

## Arguments

- spec_or_fit:

  mixgpd_fit or spec list

## Value

List with d/p/q/r functions and bulk_params.
