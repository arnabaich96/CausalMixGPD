# Run MCMC for a causal bundle

Executes the outcome arms (and PS model if enabled), returning a single
causal fit. When `PS=FALSE` in the bundle, the PS model is skipped
entirely.

## Usage

``` r
run_mcmc_causal(bundle, show_progress = TRUE)
```

## Arguments

- bundle:

  A `"dpmixgpd_causal_bundle"` from
  [`build_causal_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_causal_bundle.md).

- show_progress:

  Logical; passed to nimble for each block.

## Value

A list of class `"dpmixgpd_causal_fit"`.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal")
fit <- run_mcmc_causal(cb)
} # }
```
