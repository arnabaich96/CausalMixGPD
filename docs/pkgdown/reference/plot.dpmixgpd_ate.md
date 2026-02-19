# Plot ATE results

Generates visualizations for average treatment effects. The `type`
parameter controls the plot style:

- `"both"` (default): Returns a list with both `trt_control` (treated vs
  control means) and `treatment_effect` (ATE curve) plots

- `"effect"`: ATE curve/points vs index/PS with CI ribbon/bars

- `"arms"`: Treated mean vs control mean, with CI ribbons

## Usage

``` r
# S3 method for class 'dpmixgpd_ate'
plot(
  x,
  y = NULL,
  type = c("both", "effect", "arms"),
  plotly = getOption("DPmixGPD.plotly", FALSE),
  ...
)
```

## Arguments

- x:

  Object of class `dpmixgpd_ate`.

- y:

  Ignored.

- type:

  Character; plot type: `"both"` (default), `"effect"`, or `"arms"`.

- plotly:

  Logical; if `TRUE`, convert the `ggplot2` output to a `plotly` /
  `htmlwidget` representation via `.wrap_plotly()`. Defaults to
  `getOption("DPmixGPD.plotly", FALSE)`.

- ...:

  Additional arguments passed to ggplot2 functions.

## Value

A list of ggplot objects with elements `trt_control` and
`treatment_effect` (if `type="both"`), or a single ggplot object (if
`type` is `"effect"` or `"arms"`).

## Examples

``` r
if (FALSE) { # \dontrun{
ate_result <- cate(fit, newdata = X_new, interval = "credible")
plot(ate_result)  # default: returns list with both plots
plot(ate_result, type = "effect")  # single ATE plot
plot(ate_result, type = "arms")    # single arms plot
} # }
```
