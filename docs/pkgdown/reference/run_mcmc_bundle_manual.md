# Run posterior sampling for a prepared one-arm bundle

`run_mcmc_bundle_manual()` is the explicit runner for objects created by
[`build_nimble_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md).
It compiles the stored NIMBLE code, executes MCMC, and returns a
`"mixgpd_fit"` object.

## Usage

``` r
run_mcmc_bundle_manual(
  bundle,
  show_progress = TRUE,
  quiet = FALSE,
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

  Logical; if TRUE, print step messages and render progress where
  supported.

- quiet:

  Logical; if TRUE, suppress console status messages. Set to FALSE to
  see progress messages during MCMC setup and execution.

- parallel_chains:

  Logical; run chains concurrently when `nchains > 1`.

- workers:

  Optional integer number of workers for parallel execution.

- timing:

  Logical; if TRUE, include stage timings (`build`, `compile`, `mcmc`)
  in `fit$timing`.

- z_update_every:

  Integer \>= 1 controlling latent cluster-label update cadence.

## Value

A fitted object of class `"mixgpd_fit"` containing posterior draws,
model metadata, and cached objects used by downstream S3 methods.

## Details

The resulting fit supports posterior summaries of the model parameters
as well as posterior predictive functionals such as \\f(y \mid x)\\,
\\S(y \mid x)\\, \\Q(\tau \mid x)\\, and restricted means.

If `parallel_chains = TRUE`, chains are run concurrently when the stored
MCMC configuration uses more than one chain. If the bundle was built
with latent cluster labels monitored, the `z_update_every` argument
controls how frequently those latent indicators are refreshed during
sampling.

## See also

[`build_nimble_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`mcmc`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md),
[`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md).

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
