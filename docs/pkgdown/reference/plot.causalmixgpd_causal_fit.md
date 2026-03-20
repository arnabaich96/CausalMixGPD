# Plot the treated and control outcome fits from a causal model

`plot.causalmixgpd_causal_fit()` is a convenience router to the
underlying one-arm diagnostic plots for the treated and control fits.

## Usage

``` r
# S3 method for class 'causalmixgpd_causal_fit'
plot(x, arm = "both", ...)
```

## Arguments

- x:

  A `"causalmixgpd_causal_fit"` object.

- arm:

  Integer or character; `1` or `"treated"` for treatment, `0` or
  `"control"` for control.

- ...:

  Additional arguments forwarded to the underlying outcome plot method.

## Value

The result of the underlying plot call (invisibly).

## See also

[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`plot.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.mixgpd_fit.md).
