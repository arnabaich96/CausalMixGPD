# Print an ATE-style effect object

`print.causalmixgpd_ate()` prints a compact summary for objects produced
by
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`att`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
or
[`ate_rmean`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate_rmean.md).

## Usage

``` r
# S3 method for class 'causalmixgpd_ate'
print(x, digits = 3, max_rows = 6, ...)
```

## Arguments

- x:

  A `"causalmixgpd_ate"` object from
  [`ate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md).

- digits:

  Number of digits to display.

- max_rows:

  Maximum number of estimate rows to display.

- ...:

  Unused.

## Value

The object `x`, invisibly.

## See also

[`summary.causalmixgpd_ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_ate.md),
[`plot.causalmixgpd_ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.causalmixgpd_ate.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
a <- ate(fit, interval = "credible")
print(a)
} # }
```
