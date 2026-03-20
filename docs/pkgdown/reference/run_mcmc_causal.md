# Run posterior sampling for a causal bundle

`run_mcmc_causal()` executes the PS block (when enabled) and the two
arm-specific outcome models prepared by
[`build_causal_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_causal_bundle.md),
then returns a single `"causalmixgpd_causal_fit"` object.

## Usage

``` r
run_mcmc_causal(
  bundle,
  show_progress = TRUE,
  quiet = FALSE,
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

  Logical; if TRUE, print step messages and render progress where
  supported.

- quiet:

  Logical; if TRUE, suppress step messages and progress display.

- parallel_arms:

  Logical; if TRUE, run control and treated outcome arms in parallel.

- workers:

  Optional integer workers for parallel arm execution.

- timing:

  Logical; if TRUE, return arm and total timings in `$timing`.

- z_update_every:

  Integer \>= 1 passed to arm-level outcome MCMC.

## Value

A list of class `"causalmixgpd_causal_fit"` containing the fitted
treated/control outcome models, optional PS fit, the original bundle,
and timing metadata when requested.

## Details

The fitted object contains the posterior draws needed to evaluate
arm-level predictive distributions \\F_1(y \mid x, \mathcal{D})\\ and
\\F_0(y \mid x, \mathcal{D})\\, followed by marginal or conditional
causal contrasts. When `PS = FALSE` in the bundle, the PS block is
skipped and outcome prediction uses only the original covariates.

## See also

[`build_causal_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_causal_bundle.md),
[`mcmc`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/mcmc.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal")
fit <- run_mcmc_causal(cb)
} # }
```
