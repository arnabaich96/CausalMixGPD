# Resolve kernel dispatch functions (scalar) Dispatch returns raw scalar nimbleFunctions for codegen; do not wrap.

Resolve kernel dispatch functions (scalar) Dispatch returns raw scalar
nimbleFunctions for codegen; do not wrap.

## Usage

``` r
.get_dispatch_scalar(spec_or_fit, backend_override = NULL, gpd_override = NULL)
```

## Arguments

- spec_or_fit:

  mixgpd_fit or spec list

## Value

List with d/p/q/r/mean/mean_trunc functions and bulk_params.

## Details

This helper resolves the density, distribution, quantile,
random-generation, and mean functions implied by a kernel, backend, and
GPD setting. The result is intentionally scalar and wrapper-free because
it is used in code-generation contexts where NIMBLE expects raw function
objects rather than vectorized R adapters.
