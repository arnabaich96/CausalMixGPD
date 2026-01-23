# Initialize kernel registries

Creates/refreshes registries used by the model specification compiler
and code generators. Each kernel entry stores bulk parameters, supports,
default regression/link behavior, and distribution signatures for SB/CRP
backends.

## Usage

``` r
init_kernel_registry()
```

## Value

Invisibly returns TRUE.

## Examples

``` r
init_kernel_registry()
#> Error in assign("kernel_registry", kernel_registry, envir = ns): cannot add bindings to a locked environment
reg <- get_kernel_registry()
#> Error in get("kernel_registry", envir = asNamespace("DPmixGPD")): object 'kernel_registry' not found
names(reg)
#> Error: object 'reg' not found
tail <- get_tail_registry()
#> Error in get("tail_registry", envir = asNamespace("DPmixGPD")): object 'tail_registry' not found
tail$params
#> Error in tail$params: object of type 'closure' is not subsettable
```
