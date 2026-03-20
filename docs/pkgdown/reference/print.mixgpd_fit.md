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
