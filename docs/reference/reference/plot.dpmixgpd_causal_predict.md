# Plot causal prediction outputs

S3 method for visualizing causal predictions from
[`predict.dpmixgpd_causal_fit()`](https://arnabaich96.github.io/DPmixGPD/reference/predict.dpmixgpd_causal_fit.md).
For mean/quantile, plots treated/control and treatment effect versus PS
(or index). For density/prob, plots treated/control values versus y.

## Usage

``` r
# S3 method for class 'dpmixgpd_causal_predict'
plot(x, y = NULL, ...)
```

## Arguments

- x:

  Object of class `dpmixgpd_causal_predict`.

- y:

  Ignored.

- ...:

  Additional arguments passed to ggplot2 functions.

## Value

A ggplot object or a list of ggplot objects.
