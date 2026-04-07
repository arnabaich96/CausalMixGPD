# Get kernel registry

Get kernel registry

## Usage

``` r
get_kernel_registry()
```

## Value

A list of kernel metadata.

## Details

This accessor returns the registry created by
[`init_kernel_registry()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/init_kernel_registry.md).
The returned object is a named list keyed by kernel name. Each kernel
definition describes which bulk parameters are present, how those
parameters may depend on covariates, whether a GPD tail is allowed, and
which density or mean functions should be dispatched for the supported
backends.

Downstream builders treat this registry as the authoritative source of
kernel-specific implementation metadata. Reading it is appropriate when
you need to inspect what the package believes a kernel can do before
constructing or debugging a model specification.

## Examples

``` r
init_kernel_registry()
reg <- get_kernel_registry()
reg$normal$bulk_params
#> [1] "mean" "sd"  
```
