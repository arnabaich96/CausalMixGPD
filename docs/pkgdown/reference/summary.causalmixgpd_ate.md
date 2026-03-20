# Summarize an ATE-style effect object

`summary.causalmixgpd_ate()` converts ATE, ATT, CATE, or restricted-mean
output into a tabular summary suitable for reporting.

## Usage

``` r
# S3 method for class 'causalmixgpd_ate'
summary(object, ...)
```

## Arguments

- object:

  A `"causalmixgpd_ate"` object from
  [`ate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md).

- ...:

  Unused.

## Value

An object of class `"summary.causalmixgpd_ate"` containing summary
statistics.

## See also

[`print.causalmixgpd_ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.causalmixgpd_ate.md),
[`plot.causalmixgpd_ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.causalmixgpd_ate.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
a <- ate(fit, interval = "credible")
summary(a)
} # }
```
