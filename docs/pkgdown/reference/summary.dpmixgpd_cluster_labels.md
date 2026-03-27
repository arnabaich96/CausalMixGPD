# Summarize cluster labels

Summarize a representative clustering for training data or new
observations.

## Usage

``` r
# S3 method for class 'dpmixgpd_cluster_labels'
summary(object, top_n = 5L, order_by = c("size", "label"), vars = NULL, ...)
```

## Arguments

- object:

  Cluster labels object.

- top_n:

  Number of populated clusters to profile when attached data are
  available.

- order_by:

  Ordering rule for descriptive cluster profiles: by decreasing cluster
  size or by label.

- vars:

  Optional character vector of numeric columns to summarize within each
  cluster.

- ...:

  Unused.

## Value

Summary list containing cluster sizes, optional cluster-level
descriptive summaries, and, when available, assignment-certainty
summaries.

## Details

If score or probability matrices are attached, certainty is summarized
by the rowwise maxima \\\max_k p\_{ik}\\, which quantify how strongly
each observation is assigned to its selected cluster. When the labels
object also carries attached training or prediction data, the summary
includes descriptive mean/sd profiles for the first populated clusters.

## See also

[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md),
[`plot.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md),
[`summary.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_fit.md).

Other cluster workflow:
[`dpmgpd.cluster`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.cluster.md)`()`,
[`dpmix.cluster`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md)`()`,
[`plot.dpmixgpd_cluster_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_bundle.md)`()`,
[`plot.dpmixgpd_cluster_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_fit.md)`()`,
[`plot.dpmixgpd_cluster_labels`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md)`()`,
[`plot.dpmixgpd_cluster_psm`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_psm.md)`()`,
[`predict.dpmixgpd_cluster_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md)`()`,
[`print.dpmixgpd_cluster_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_bundle.md)`()`,
[`print.dpmixgpd_cluster_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_fit.md)`()`,
[`print.dpmixgpd_cluster_labels`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_labels.md)`()`,
[`print.dpmixgpd_cluster_psm`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_psm.md)`()`,
[`summary.dpmixgpd_cluster_bundle`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_bundle.md)`()`,
[`summary.dpmixgpd_cluster_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_fit.md)`()`,
[`summary.dpmixgpd_cluster_psm`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_psm.md)`()`
