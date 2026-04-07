# Print a causal-model summary object

Print a causal-model summary object

## Usage

``` r
# S3 method for class 'summary.causalmixgpd_causal_fit'
print(x, digits = 3, max_rows = 60, ...)
```

## Arguments

- x:

  A `"summary.causalmixgpd_causal_fit"` object.

- digits:

  Number of digits to print in summary tables.

- max_rows:

  Maximum rows to print from each summary table.

- ...:

  Unused.

## Value

`x` invisibly.

## Details

This is a formatter for the object returned by
[`summary.causalmixgpd_causal_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_causal_fit.md).
It prints the propensity-score summary first when that block is present,
followed by the control and treated outcome summaries on the same scale
of posterior diagnostics.

No new computation is performed here. The method simply organizes the
stored summary tables so that the three fitted blocks can be inspected
together.
