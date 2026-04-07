# Print an ATE summary

Print an ATE summary

## Usage

``` r
# S3 method for class 'summary.causalmixgpd_ate'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A `"summary.causalmixgpd_ate"` object.

- digits:

  Number of digits to display.

- ...:

  Unused.

## Value

The object `x`, invisibly.

## Details

This method formats the object returned by
[`summary.causalmixgpd_ate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_ate.md).
It prints the prediction design, interval settings, optional model
metadata, and the resulting treatment-effect table on the mean or
restricted-mean scale.

The method is purely a reporting layer. All posterior aggregation has
already been completed by the corresponding summary constructor.
