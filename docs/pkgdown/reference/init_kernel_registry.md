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

## Details

The kernel registry is the package-level contract that keeps model
building, prediction, and documentation aligned. Each entry records the
natural bulk parameters for one kernel, the support constraints they
must satisfy, the default covariate-link strategy, and the
backend-specific distribution names used when generating NIMBLE code.

The companion tail registry records the generalized Pareto tail
parameters \\u\\ (threshold), \\\sigma_u\\ (tail scale), and \\\xi_u\\
(tail shape) together with their support and allowed modeling modes.
Calling `init_kernel_registry()` makes those contracts available in the
package namespace so later builders can validate requests without
duplicating lookup logic.

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
