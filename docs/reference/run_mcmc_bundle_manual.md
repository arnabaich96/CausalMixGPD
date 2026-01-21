# Run MCMC for a prepared bundle

Run MCMC for a prepared bundle

## Usage

``` r
run_mcmc_bundle_manual(bundle, show_progress = TRUE)
```

## Arguments

- bundle:

  A `dpmixgpd_bundle` from
  [`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md).

- show_progress:

  Logical; passed to nimble.

## Value

A fitted object of class `"mixgpd_fit"`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(nimble)
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
fit
} # }
```
