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
#> Error in assign("kernel_registry", kernel_registry, envir = ns): cannot change value of locked binding for 'kernel_registry'
tail <- get_tail_registry()
tail$params
#> [1] "threshold"  "tail_scale" "tail_shape"
```
