# Plot a cluster fit

Visualize either the posterior similarity matrix, the posterior number
of occupied clusters, the size distribution of the representative
clusters, or cluster-specific response summaries.

## Usage

``` r
# S3 method for class 'dpmixgpd_cluster_fit'
plot(
  x,
  which = c("psm", "k", "sizes", "summary"),
  burnin = NULL,
  thin = NULL,
  psm_max_n = 2000L,
  top_n = 5L,
  order_by = c("size", "label"),
  plotly = getOption("CausalMixGPD.plotly", FALSE),
  ...
)
```

## Arguments

- x:

  A cluster fit.

- which:

  Plot type:

  - `"psm"`: posterior similarity matrix heatmap

  - `"k"`: posterior number of occupied clusters

  - `"sizes"`: bar chart of representative cluster sizes

  - `"summary"`: cluster-specific response summaries

- burnin:

  Number of initial posterior draws to discard.

- thin:

  Keep every `thin`-th posterior draw.

- psm_max_n:

  Maximum training sample size allowed for PSM plotting.

- top_n:

  Number of populated representative clusters to display for
  `which = "sizes"` or `which = "summary"`. Use `NULL` to display all
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

This plot method exposes the main posterior diagnostics for clustering.
The `which = "k"` view tracks the number of occupied clusters across
retained draws, `which = "psm"` visualizes pairwise co-clustering
probabilities, `which = "sizes"` displays the size profile of the
representative partition, and `which = "summary"` shows response
summaries conditional on the selected representative labels.

The representative partition is obtained from
[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md)
using Dahl's least-squares rule. As a result, the `sizes` and `summary`
views describe that representative clustering rather than the full
posterior distribution over partitions.

## See also

[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md),
[`summary.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_fit.md),
[`plot.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_psm.md),
[`plot.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md).

Other cluster workflow:
[`dpmgpd.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.cluster.md),
[`dpmix.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md),
[`plot.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_bundle.md),
[`plot.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md),
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
