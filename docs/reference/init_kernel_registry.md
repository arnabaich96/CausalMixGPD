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
reg <- get_kernel_registry()
names(reg)
#> [1] "normal"    "lognormal" "invgauss"  "gamma"     "laplace"   "amoroso"  
#> [7] "cauchy"   
tail <- get_tail_registry()
tail$params
#> [1] "threshold"  "tail_scale" "tail_shape"
```
