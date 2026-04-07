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

## Details

Each arm-specific outcome model is itself a `mixgpd_fit`, so this method
delegates to
[`plot.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.mixgpd_fit.md)
for the selected arm. With `arm = "both"`, it returns a named list of
treated and control diagnostics so the two fitted outcome models can be
assessed side by side.

These are MCMC diagnostics for the nuisance outcome models, not plots of
causal estimands. Use
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on objects from
[`predict.causalmixgpd_causal_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`qte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
or
[`ate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md)
when the goal is to visualize treatment effects rather than chain
behavior.

## See also

[`plot.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.mixgpd_fit.md),
[`predict.causalmixgpd_causal_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.causalmixgpd_causal_fit.md),
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md).
