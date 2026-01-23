# Get tail registry

Get tail registry

## Usage

``` r
get_tail_registry()
```

## Value

A list of tail metadata.

## Examples

``` r
init_kernel_registry()
#> Error in assign("kernel_registry", kernel_registry, envir = ns): cannot add bindings to a locked environment
tail <- get_tail_registry()
#> Error in get("tail_registry", envir = asNamespace("DPmixGPD")): object 'tail_registry' not found
tail$params
#> Error in tail$params: object of type 'closure' is not subsettable
```
