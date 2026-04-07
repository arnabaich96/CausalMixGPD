# Plot cluster labels

Visualize representative cluster sizes, assignment certainty, or
cluster-specific response summaries. For `type = "summary"`, the
response view is shown as boxplots ordered by cluster size or label.
When `x` comes from `predict(..., newdata = ...)`, only clusters
represented in the new sample are displayed.

## Usage

``` r
# S3 method for class 'dpmixgpd_cluster_labels'
plot(
  x,
  type = c("sizes", "certainty", "summary"),
  top_n = 5L,
  order_by = c("size", "label"),
  plotly = getOption("CausalMixGPD.plotly", FALSE),
  ...
)
```

## Arguments

- x:

  Cluster labels object.

- type:

  Plot type:

  - `"sizes"`: bar chart of representative cluster sizes

  - `"certainty"`: assignment certainty distribution

  - `"summary"`: cluster-specific response boxplots

- top_n:

  Number of populated representative clusters to display for
  `type = "sizes"` or `type = "summary"`. Use `NULL` to display all
  populated clusters.

- order_by:

  Ordering rule for cluster displays:

  - `"size"`: decreasing cluster size

  - `"label"`: ascending cluster label

- plotly:

  Logical; if `TRUE`, convert the `ggplot2` output to a `plotly` /
  `htmlwidget` representation via `.wrap_plotly()`. Defaults to
  `getOption("CausalMixGPD.plotly", FALSE)`.

- ...:

  Unused.

## Value

A `ggplot2` object or a `plotly`/`htmlwidget` object when
`plotly = TRUE`.

## Details

This method visualizes the representative partition stored in a
`dpmixgpd_cluster_labels` object. The `sizes` view emphasizes the
empirical distribution of the selected clusters, the `certainty` view
summarizes the assignment scores \\\max_k p\_{ik}\\, and the `summary`
view compares the attached response data across representative clusters.

For new-data prediction, the plots are always interpreted relative to
the representative training clusters. That is why only clusters observed
in the predicted sample are shown even though the training partition may
contain additional occupied groups.

## See also

[`summary.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md),
[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md).

Other cluster workflow:
[`dpmgpd.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.cluster.md),
[`dpmix.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md),
[`plot.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_bundle.md),
[`plot.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_fit.md),
[`plot.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_psm.md),
[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md),
[`print.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_bundle.md),
[`print.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_fit.md),
[`print.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_labels.md),
[`print.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_psm.md),
[`summary.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_bundle.md),
[`summary.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_fit.md),
[`summary.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md),
[`summary.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_psm.md)
