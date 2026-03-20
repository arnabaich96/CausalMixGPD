# Summarize a one-arm workflow bundle

`summary.causalmixgpd_bundle()` prints the structural contents of a
bundle before MCMC is run.

## Usage

``` r
# S3 method for class 'causalmixgpd_bundle'
summary(object, ...)
```

## Arguments

- object:

  A `"causalmixgpd_bundle"` object.

- ...:

  Unused.

## Value

An invisible list with elements `meta`, `priors`, and `monitors`.

## Details

The summary is meant for workflow validation rather than inference. It
shows:

- the model metadata (backend, kernel, components, covariates, GPD
  flag),

- the prior/parameter table derived from `spec$plan`,

- the nodes that will be monitored during MCMC.

This is the recommended checkpoint after
[`build_nimble_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md)
and before
[`run_mcmc_bundle_manual`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_bundle_manual.md).

## See also

[`build_nimble_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`print.causalmixgpd_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.causalmixgpd_bundle.md),
[`run_mcmc_bundle_manual`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_bundle_manual.md).

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = FALSE, components = 6)
summary(bundle)
} # }
```
