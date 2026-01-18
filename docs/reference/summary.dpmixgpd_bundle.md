# Summarize a dpmixgpd bundle

User-facing summary for pre-run bundles produced by
[`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md).
This prints:

- meta information (backend, kernel, components, N, covariates, GPD
  flag)

- a readable prior/parameter table derived from `spec$plan`

- monitor set overview

## Usage

``` r
# S3 method for class 'dpmixgpd_bundle'
summary(object, ...)
```

## Arguments

- object:

  A `"dpmixgpd_bundle"` object.

- ...:

  Unused.

## Value

An invisible list with elements `meta`, `priors`, `monitors`.

## Examples

``` r
# \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = FALSE, components = 6)
summary(bundle)
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Normal Distribution
#>  Components                      6
#>           N                     50
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                     sb     
#>           meta     kernel info           model                 normal     
#>           meta components info           model                      6     
#>           meta          N info           model                     50     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk       mean dist component (1:6)   normal(mean=0, sd=5)     
#>           bulk         sd dist component (1:6) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:6], z[1:50], mean[1:6], sd[1:6]
#> 
# }
```
