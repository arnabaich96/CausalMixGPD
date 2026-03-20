# Print a QTE-style effect object

`print.causalmixgpd_qte()` prints a compact summary for objects produced
by
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`qtt`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qtt.md),
or
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

## Usage

``` r
# S3 method for class 'causalmixgpd_qte'
print(x, digits = 3, max_rows = 6, ...)
```

## Arguments

- x:

  A `"causalmixgpd_qte"` object from
  [`qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md).

- digits:

  Number of digits to display.

- max_rows:

  Maximum number of estimate rows to display.

- ...:

  Unused.

## Value

The object `x`, invisibly.

## See also

[`summary.causalmixgpd_qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_qte.md),
[`plot.causalmixgpd_qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.causalmixgpd_qte.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, A = A, backend = "sb", kernel = "normal", components = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
print(q)
} # }
```
