# Print a QTE summary

Print a QTE summary

## Usage

``` r
# S3 method for class 'summary.causalmixgpd_qte'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A `"summary.causalmixgpd_qte"` object.

- digits:

  Number of digits to display.

- ...:

  Unused.

## Value

The object `x`, invisibly.

## Details

This formatter displays the summary object returned by
[`summary.causalmixgpd_qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_qte.md).
It reports the quantile grid, interval configuration, model metadata
when available, and the tabulated quantile effect summaries.

No additional causal computations are performed here. The method simply
turns the stored summary tables into a readable report.
