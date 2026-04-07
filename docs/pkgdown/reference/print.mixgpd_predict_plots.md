# Print method for prediction plots

Print method for prediction plots

## Usage

``` r
# S3 method for class 'mixgpd_predict_plots'
print(x, ...)
```

## Arguments

- x:

  Object of class `mixgpd_predict_plots`.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns the input object.

## Details

Prediction plotting methods may return a single plot or a richer plot
object with an additional wrapper class. This print method temporarily
drops that wrapper class so the underlying graphics object uses its
native print method.

The stored predictive summaries are not changed.
