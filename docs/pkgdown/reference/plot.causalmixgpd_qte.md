# Plot QTE results

Generates visualizations for quantile treatment effects. The `type`
parameter controls the plot style:

- `"both"` (default): Returns a list with both `trt_control` (treated vs
  control quantile curves) and `treatment_effect` (QTE curve) plots

- `"effect"`: QTE curve vs quantile levels (`probs`) with CI ribbon

- `"arms"`: Treated and control quantile curves vs `probs`, with CI
  ribbons

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

## Examples

``` r
if (FALSE) { # \dontrun{
qte_result <- cqte(fit, probs = c(0.1, 0.5, 0.9), newdata = X_new)
plot(qte_result)  # default: returns list with both plots
plot(qte_result, type = "effect")  # single QTE plot
plot(qte_result, type = "arms")    # single arms plot
} # }
```
