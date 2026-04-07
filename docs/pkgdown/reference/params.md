# Extract posterior mean parameters in natural shape

`params()` reshapes posterior mean summaries back into the parameter
layout implied by the fitted model specification.

## Usage

``` r
params(object, ...)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- ...:

  Unused.

## Value

An object of class `"mixgpd_params"` (a named list). For causal fits,
`params()` returns a treated/control pair and includes a `ps` block when
a propensity-score model was fitted.

## Details

This extractor is intended for structural inspection of the fitted
model. Scalar quantities remain scalar, component-specific parameters
are returned as vectors, and linked regression blocks are returned as
matrices with covariate names as columns when available. If
propensity-score adjustment is active for a linked bulk parameter, its
coefficient is folded into the returned beta matrix as a leading
`"PropScore"` column.

For a spliced model, the extractor returns posterior means of the bulk
mixture parameters together with component-level threshold, tail-scale,
and tail-shape terms. When tail terms are link-mode, the corresponding
component-by-covariate beta blocks are returned.

## See also

[`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md),
[`ess_summary`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ess_summary.md).

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
params(fit)
p <- params(fit)
} # }
```
