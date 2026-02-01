# Resolve kernel dispatch functions (scalar) Dispatch returns raw scalar nimbleFunctions for codegen; do not wrap.

Resolve kernel dispatch functions (scalar) Dispatch returns raw scalar
nimbleFunctions for codegen; do not wrap.

## Usage

``` r
.get_dispatch_scalar(spec_or_fit, backend_override = NULL)
```

## Arguments

- spec_or_fit:

  mixgpd_fit or spec list

## Value

List with d/p/q/r functions and bulk_params.
