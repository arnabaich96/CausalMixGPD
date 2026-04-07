# Print method for fitted value plots

Print method for fitted value plots

## Usage

``` r
# S3 method for class 'mixgpd_fitted_plots'
print(x, ...)
```

## Arguments

- x:

  Object of class `mixgpd_fitted_plots`.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns the input object.

## Details

The fitted-value diagnostic object stores two plot panels. This print
method renders them in sequence so both the observed-versus-fitted
comparison and the residual-versus-fitted comparison are shown together.

It is a display helper only and does not recompute fitted values or
residuals.
