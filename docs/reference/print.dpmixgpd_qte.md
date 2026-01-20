# Print a QTE object

User-facing print method for `"dpmixgpd_qte"` objects produced by
[`qte()`](https://arnabaich96.github.io/DPmixGPD/reference/qte.md).
Displays a compact summary: prediction points, quantile grid, credible
level, and the first few rows of QTE estimates.

## Usage

``` r
# S3 method for class 'dpmixgpd_qte'
print(x, digits = 3, max_rows = 6, ...)
```

## Arguments

- x:

  A `"dpmixgpd_qte"` object from
  [`qte()`](https://arnabaich96.github.io/DPmixGPD/reference/qte.md).

- digits:

  Number of digits to display.

- max_rows:

  Maximum number of estimate rows to display.

- ...:

  Unused.

## Value

The object `x`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
cb <- build_causal_bundle(y = y, X = X, T = T, backend = "sb", kernel = "normal", J = 6)
fit <- run_mcmc_causal(cb, show_progress = FALSE)
q <- qte(fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
print(q)
} # }
```
