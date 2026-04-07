# Plot fitted values diagnostics

S3 method for visualizing fitted values from
[`fitted.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/fitted.mixgpd_fit.md).
Produces a 2-panel figure: Q-Q plot and residuals vs fitted.

## Usage

``` r
# S3 method for class 'mixgpd_fitted'
plot(x, y = NULL, ...)
```

## Arguments

- x:

  Object of class `mixgpd_fitted` from
  [`fitted.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/fitted.mixgpd_fit.md).

- y:

  Ignored; included for S3 compatibility.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns a list with the two plots.

## Details

These diagnostics compare the fitted values implied by the posterior
summary on the training design against the observed responses. The first
panel checks how closely fitted and observed values align, while the
second panel looks for residual structure that would indicate lack of
fit or remaining mean trends.

This method is distinct from posterior predictive simulation on new
data. It is a training-sample diagnostic built from
[`fitted.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/fitted.mixgpd_fit.md)
and the corresponding residuals.
