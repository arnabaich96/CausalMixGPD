# Run posterior sampling from a prepared bundle

`mcmc()` is the generic workflow runner. It dispatches to
[`run_mcmc_bundle_manual`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_bundle_manual.md)
for one-arm bundles and to
[`run_mcmc_causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md)
for causal bundles.

## Usage

``` r
mcmc(b, ...)
```

## Arguments

- b:

  A non-causal or causal bundle.

- ...:

  Optional MCMC overrides (`niter`, `nburnin`, `thin`, `nchains`,
  `seed`, `waic`) and runner controls (`show_progress`, `quiet`).

## Value

A fitted object of class `"mixgpd_fit"` or `"causalmixgpd_causal_fit"`.

## Details

This wrapper is useful when you want a two-stage workflow: build first,
inspect or modify the bundle, then sample. Named MCMC arguments supplied
through `...` override the settings stored in the bundle before
execution.

The returned fit represents posterior draws from the finite SB/CRP
approximation encoded in the bundle. Downstream summaries therefore
target posterior predictive quantities such as \\f(y \mid x)\\, \\F(y
\mid x)\\, and derived treatment-effect functionals.

## See also

[`bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`run_mcmc_bundle_manual`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_bundle_manual.md),
[`run_mcmc_causal`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/run_mcmc_causal.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md).
