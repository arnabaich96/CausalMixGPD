# Print an ATE object

User-facing print method for `"dpmixgpd_ate"` objects produced by
[`ate()`](https://arnabaich96.github.io/DPmixGPD/reference/ate.md).
Displays a compact summary: prediction points, credible level, and the
first few ATE estimates.

## Usage

``` r
# S3 method for class 'dpmixgpd_ate'
print(x, digits = 3, max_rows = 6, ...)
```

## Arguments

- x:

  A `"dpmixgpd_ate"` object from
  [`ate()`](https://arnabaich96.github.io/DPmixGPD/reference/ate.md).

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
a <- ate(fit, interval = "credible")
print(a)
} # }
```
