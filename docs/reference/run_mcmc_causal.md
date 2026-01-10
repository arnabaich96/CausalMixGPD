# Run MCMC for a causal bundle

Executes the PS model and both outcome arms, returning a single causal
fit.

## Usage

``` r
run_mcmc_causal(bundle, show_progress = TRUE)
```

## Arguments

- bundle:

  A `"dpmixgpd_causal_bundle"` from
  [`build_causal_bundle()`](https://example.com/DPmixGPD/reference/build_causal_bundle.md).

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
