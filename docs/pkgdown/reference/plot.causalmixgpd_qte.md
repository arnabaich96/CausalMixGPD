# Plot QTE-style effect summaries

`plot.causalmixgpd_qte()` visualizes objects returned by
[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`qtt`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qtt.md),
and
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md).
The `type` parameter controls the plot style. When `type` is omitted,
[`cqte()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md)
objects default to `"effect"` and, when multiple quantile levels are
present, `facet_by = "id"`. Whenever quantile index appears on the
x-axis, it is shown as an ordered categorical axis with equidistant
spacing:

- `"both"` (default): Returns a list with both `trt_control` (treated vs
  control quantile curves) and `treatment_effect` (QTE curve) plots

- `"effect"`: QTE curve vs quantile levels (`probs`) with pointwise CI
  error bars

- `"arms"`: Treated and control quantile curves vs `probs`, with
  pointwise CI error bars

## Usage

``` r
# S3 method for class 'causalmixgpd_qte'
plot(
  x,
  y = NULL,
  type = c("both", "effect", "arms"),
  facet_by = c("tau", "id"),
  plotly = getOption("CausalMixGPD.plotly", FALSE),
  ...
)
```

## Arguments

- x:

  Object of class `causalmixgpd_qte`.

- y:

  Ignored.

- type:

  Character; plot type: `"both"` (default), `"effect"`, or `"arms"`.

- facet_by:

  Character; faceting strategy when multiple prediction points exist.
  `"tau"` (default) facets by quantile level, `"id"` facets by
  prediction point.

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

## See also

[`qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/qte.md),
[`cqte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cqte.md),
[`summary.causalmixgpd_qte`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.causalmixgpd_qte.md).

## Examples

``` r
if (FALSE) { # \dontrun{
qte_result <- cqte(fit, probs = c(0.1, 0.5, 0.9), newdata = X_new)
plot(qte_result)  # CQTE default: effect plot (faceted by id when needed)
plot(qte_result, type = "effect")  # single QTE plot
plot(qte_result, type = "arms")    # single arms plot
} # }
```
