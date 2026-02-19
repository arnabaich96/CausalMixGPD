# Run MCMC for a causal bundle

Executes the outcome arms (and PS model if enabled), returning a single
causal fit. When `PS=FALSE` in the bundle, the PS model is skipped
entirely.

## Usage

``` r
run_mcmc_causal(
  bundle,
  show_progress = TRUE,
  parallel_arms = FALSE,
  workers = NULL,
  timing = FALSE,
  z_update_every = NULL
)
```

## Arguments

- bundle:

  A `"causalmixgpd_causal_bundle"` from
  [`build_causal_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_causal_bundle.md).

- show_progress:

  Logical; passed to nimble for each block.

- parallel_arms:

  Logical; if TRUE, run control and treated outcome arms in parallel.

- workers:

  Optional integer workers for parallel arm execution.

- timing:

  Logical; if TRUE, return arm and total timings in `$timing`.

- z_update_every:

  Integer \>= 1 passed to arm-level outcome MCMC.

## Value

A list of class `"causalmixgpd_causal_fit"`.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
fit <- run_mcmc_causal(cb)
} # }
```
