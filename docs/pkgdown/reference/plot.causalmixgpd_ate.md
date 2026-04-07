# Plot ATE-style effect summaries

`plot.causalmixgpd_ate()` visualizes objects returned by
[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`att`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/att.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
and
[`ate_rmean`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate_rmean.md).
The `type` parameter controls the plot style. When `type` is omitted,
[`cate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md)
objects default to `"effect"`:

- `"both"` (default): Returns a list with both `trt_control` (treated vs
  control means) and `treatment_effect` (ATE curve) plots

- `"effect"`: ATE curve/points vs index/PS with pointwise CI error bars

- `"arms"`: Treated mean vs control mean, with pointwise CI error bars

## Usage

``` r
# S3 method for class 'causalmixgpd_ate'
plot(
  x,
  y = NULL,
  type = c("both", "effect", "arms"),
  plotly = getOption("CausalMixGPD.plotly", FALSE),
  ...
)
```

## Arguments

- x:

  Object of class `causalmixgpd_ate`.

- y:

  Ignored.

- type:

  Character; plot type:

  - `"both"` (default): returns a list with both arm means and
    treatment-effect plots

  - `"effect"`: ATE curve/points with pointwise CI error bars

  - `"arms"`: treated vs control mean with pointwise CI error bars

- plotly:

  Logical; if `TRUE`, convert the `ggplot2` output to a `plotly` /
  `htmlwidget` representation via `.wrap_plotly()`. Defaults to
  `getOption("CausalMixGPD.plotly", FALSE)`.

- ...:

  Additional arguments passed to ggplot2 functions.

## Value

A list of ggplot objects with elements `trt_control` and
`treatment_effect` (if `type="both"`), or a single ggplot object (if
`type` is `"effect"` or `"arms"`).

## Details

The effect panel visualizes the posterior summary of the treatment
contrast on the mean scale, namely \\E(Y^1) - E(Y^0)\\ or its
conditional or treated-standardized analogue. The arms panel instead
shows the treated and control mean predictions whose difference defines
that contrast.

For
[`cate()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md)
objects, the x-axis follows the prediction profiles; otherwise it uses
the estimated propensity score when available or a simple index order.
This keeps the comparison aligned with how the effect object was
standardized.

## See also

[`ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ate.md),
[`cate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cate.md),
[`summary.causalmixgpd_ate`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_ate.md).

## Examples

``` r
if (FALSE) { # \dontrun{
ate_result <- cate(fit, newdata = X_new, interval = "credible")
plot(ate_result)  # CATE default: effect plot
plot(ate_result, type = "effect")  # single ATE plot
plot(ate_result, type = "arms")    # single arms plot
} # }
```
