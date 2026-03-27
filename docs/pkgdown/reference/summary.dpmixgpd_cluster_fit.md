# Summarize a cluster fit

Summarize the posterior clustering induced by the Dahl representative
partition.

## Usage

``` r
# S3 method for class 'dpmixgpd_cluster_fit'
summary(
  object,
  burnin = NULL,
  thin = NULL,
  top_n = 5L,
  order_by = c("size", "label"),
  vars = NULL,
  ...
)
```

## Arguments

- object:

  A cluster fit.

- burnin:

  Number of initial posterior draws to discard.

- thin:

  Keep every `thin`-th posterior draw.

- top_n:

  Number of populated clusters to profile when descriptive summaries are
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

Summary list with the number of retained clusters, cluster sizes,
optional cluster-level descriptive summaries, and the burn-in/thinning
settings used to construct the summary.

## Details

This summary is based on
[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md)
with `type = "label"`. The reported cluster count \\K^\*\\ is the number
of unique labels in the representative partition rather than the number
of components available in the truncated sampler.

## See also

[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md),
[`plot.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_fit.md),
[`summary.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md).

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
[`summary.dpmixgpd_cluster_labels`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md)`()`,
[`summary.dpmixgpd_cluster_psm`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_psm.md)`()`
