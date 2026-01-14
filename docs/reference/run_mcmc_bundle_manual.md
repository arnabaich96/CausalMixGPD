# Run MCMC for a prepared bundle

Run MCMC for a prepared bundle

## Usage

``` r
run_mcmc_bundle_manual(bundle, show_progress = TRUE)
```

## Arguments

- bundle:

  A `dpmixgpd_bundle` from
  [`build_nimble_bundle()`](https://example.com/DPmixGPD/reference/build_nimble_bundle.md).

- show_progress:

  Logical; passed to nimble.

## Value

A fitted object of class `"mixgpd_fit"`.

## Examples

``` r
library(nimble)
#> nimble version 1.4.0 is loaded.
#> For more information on NIMBLE and a User Manual,
#> please visit https://R-nimble.org.
#> 
#> Attaching package: 'nimble'
#> The following object is masked from 'package:stats':
#> 
#>     simulate
#> The following object is masked from 'package:base':
#> 
#>     declare
y <- abs(rnorm(40)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = FALSE,
  components = 3,
  mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
)
#> Error in build_nimble_bundle(y = y, backend = "sb", kernel = "normal",     GPD = FALSE, components = 3, mcmc = list(niter = 200, nburnin = 50,         thin = 1, nchains = 1, seed = 1)): could not find function "build_nimble_bundle"
fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
#> Error in run_mcmc_bundle_manual(bundle, show_progress = FALSE): could not find function "run_mcmc_bundle_manual"
fit
#> Error: object 'fit' not found
```
