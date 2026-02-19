# Run MCMC for a prepared bundle

Run MCMC for a prepared bundle

## Usage

``` r
run_mcmc_bundle_manual(
  bundle,
  show_progress = TRUE,
  quiet = TRUE,
  parallel_chains = FALSE,
  workers = NULL,
  timing = FALSE,
  z_update_every = NULL
)
```

## Arguments

- bundle:

  A `causalmixgpd_bundle` from
  [`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md).

- show_progress:

  Logical; passed to nimble.

- quiet:

  Logical; if TRUE (default), suppress console status messages. Set to
  FALSE to see progress messages during MCMC setup and execution.

- parallel_chains:

  Logical; run chains concurrently when `nchains > 1`.

- workers:

  Optional integer number of workers for parallel execution.

- timing:

  Logical; if TRUE, include stage timings (`build`, `compile`, `mcmc`)
  in `fit$timing`.

- z_update_every:

  Integer \>= 1 controlling latent allocation update cadence.

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
