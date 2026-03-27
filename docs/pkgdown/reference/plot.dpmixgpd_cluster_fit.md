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

  Plot type.

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

  Ordering rule for cluster displays: decreasing cluster size or label.

- plotly:

  Logical; if `TRUE`, convert the `ggplot2` output to a `plotly` /
  `htmlwidget` representation via `.wrap_plotly()`. Defaults to
  `getOption("CausalMixGPD.plotly", FALSE)`.

- ...:

  Unused.

## Value

A `ggplot2` object or a `plotly`/`htmlwidget` object when
`plotly = TRUE`.

## See also

[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md),
[`summary.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_fit.md),
[`plot.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_psm.md),
[`plot.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md).

Other cluster workflow:
[`dpmgpd.cluster`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.cluster.md)`()`,
[`dpmix.cluster`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md)`()`,
[`plot.dpmixgpd_cluster_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_bundle.md)`()`,
[`plot.dpmixgpd_cluster_labels`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md)`()`,
[`plot.dpmixgpd_cluster_psm`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_psm.md)`()`,
[`predict.dpmixgpd_cluster_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md)`()`,
[`print.dpmixgpd_cluster_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_bundle.md)`()`,
[`print.dpmixgpd_cluster_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_fit.md)`()`,
[`print.dpmixgpd_cluster_labels`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_labels.md)`()`,
[`print.dpmixgpd_cluster_psm`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_psm.md)`()`,
[`summary.dpmixgpd_cluster_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_bundle.md)`()`,
[`summary.dpmixgpd_cluster_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_fit.md)`()`,
[`summary.dpmixgpd_cluster_labels`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md)`()`,
[`summary.dpmixgpd_cluster_psm`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_psm.md)`()`
