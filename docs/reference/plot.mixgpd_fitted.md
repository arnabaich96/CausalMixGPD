# Plot fitted values diagnostics

S3 method for visualizing fitted values from
[`fitted.mixgpd_fit()`](https://arnabaich96.github.io/DPmixGPD/reference/fitted.mixgpd_fit.md).
Produces a 2-panel figure: Q-Q plot and residuals vs fitted.

## Usage

``` r
# S3 method for class 'mixgpd_fitted'
plot(x, y = NULL, ...)
```

## Arguments

- x:

  Object of class `mixgpd_fitted` from
  [`fitted.mixgpd_fit()`](https://arnabaich96.github.io/DPmixGPD/reference/fitted.mixgpd_fit.md).

- y:

  Ignored; included for S3 compatibility.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns a list with the two plots.
