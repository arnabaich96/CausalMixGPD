# Print a propensity-score summary object

Print a propensity-score summary object

## Usage

``` r
# S3 method for class 'summary.causalmixgpd_ps_fit'
print(x, digits = 3, max_rows = 60, show_ess = FALSE, ...)
```

## Arguments

- x:

  A `"summary.causalmixgpd_ps_fit"` object.

- digits:

  Number of digits to print in summary tables.

- max_rows:

  Maximum rows to print from the summary table.

- show_ess:

  Logical; if `TRUE`, include the `ess` column when present.

- ...:

  Unused.

## Value

`x` invisibly.

## Details

This is a display method for the object returned by
[`summary.causalmixgpd_ps_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_ps_fit.md).
It prints the PS model identity, the effective data dimension used by
that model, and the posterior summary table for the monitored
parameters.

The method does not recompute propensity scores or refit the model. It
is a formatting layer over already computed posterior summaries.
