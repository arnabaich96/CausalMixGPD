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
reg <- get_kernel_registry()
reg$normal$bulk_params
#> [1] "mean" "sd"  
```
