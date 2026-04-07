# Print method for mixgpd_fit diagnostic plots

Print method for mixgpd_fit diagnostic plots

## Usage

``` r
# S3 method for class 'mixgpd_fit_plots'
print(x, ...)
```

## Arguments

- x:

  Object of class `mixgpd_fit_plots`.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns the input object.

## Details

Diagnostic plotting for `mixgpd_fit` can return a named collection of
ggmcmc graphics. This print method iterates through that collection and
prints each stored diagnostic plot with a section label so trace,
density, and related views can be read in order.

The method performs no additional posterior computation.
