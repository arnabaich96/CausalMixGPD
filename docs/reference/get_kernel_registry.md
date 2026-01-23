# Get kernel registry

Get kernel registry

## Usage

``` r
get_kernel_registry()
```

## Value

A list of kernel metadata.

## Examples

``` r
init_kernel_registry()
#> Error in assign("kernel_registry", kernel_registry, envir = ns): cannot add bindings to a locked environment
reg <- get_kernel_registry()
#> Error in get("kernel_registry", envir = asNamespace("DPmixGPD")): object 'kernel_registry' not found
reg$normal$bulk_params
#> Error: object 'reg' not found
```
