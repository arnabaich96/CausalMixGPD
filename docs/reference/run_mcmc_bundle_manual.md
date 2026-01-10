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
fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, mean, sd, w, z
#> ===== Samplers =====
#> conjugate sampler (8)
#>   - mean[]  (3 elements)
#>   - sd[]  (3 elements)
#>   - v[]  (2 elements)
#> categorical sampler (40)
#>   - z[]  (40 elements)
#> RW sampler (1)
#>   - alpha
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] There are 4 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
fit
#> MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: FALSE
#> n = 40 | components = 3 | epsilon = 0.025
#> MCMC: niter=200, nburnin=50, thin=1, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
```
