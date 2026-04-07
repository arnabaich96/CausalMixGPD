# Print a one-arm fitted model

`print.mixgpd_fit()` gives a compact header for a fitted one-arm model.
It is meant as a quick identity check rather than a full posterior
summary.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
print(x, ...)
```

## Arguments

- x:

  A fitted object of class `"mixgpd_fit"`.

- ...:

  Unused.

## Value

`x` invisibly.

## Details

The fitted object represents posterior draws from a bulk mixture model,
or from its spliced bulk-tail extension when `GPD = TRUE`. For the bulk
part, the predictive law has the mixture form \$\$f(y \mid x) =
\sum\_{k=1}^{K} w_k(x) f_k(y \mid x, \theta_k).\$\$ When a GPD tail is
active, exceedances above the threshold are instead routed through the
generalized Pareto tail attached to the same bulk mixture.

The print method reports only the model identity and basic metadata. Use
[`summary()`](https://rdrr.io/r/base/summary.html) for parameter-level
posterior summaries, [`predict()`](https://rdrr.io/r/stats/predict.html)
for predictive functionals, and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) for chain
diagnostics.

## See also

[`summary.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md),
[`params`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/params.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md).

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
print(fit)
} # }
```
