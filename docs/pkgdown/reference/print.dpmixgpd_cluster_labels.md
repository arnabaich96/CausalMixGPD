# Print cluster labels

Compact display for a representative clustering.

## Usage

``` r
# S3 method for class 'dpmixgpd_cluster_labels'
print(x, ...)
```

## Arguments

- x:

  Cluster labels object.

- ...:

  Unused.

## Value

`x`, invisibly.

## Details

A `dpmixgpd_cluster_labels` object represents one partition, usually the
Dahl representative partition for the training data or the induced
allocation of `newdata` to those representative clusters. The printed
output therefore describes the selected labels and their sizes, not the
full posterior uncertainty over alternative partitions.

When the object comes from `predict(..., return_scores = TRUE)`, richer
assignment information is carried alongside the labels and can be
inspected with
[`summary.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md)
or
[`plot.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md).

## See also

[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md),
[`summary.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md),
[`plot.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md).

Other cluster workflow:
[`dpmgpd.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.cluster.md),
[`dpmix.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md),
[`plot.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_bundle.md),
[`plot.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_fit.md),
[`plot.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md),
[`plot.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_psm.md),
[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md),
[`print.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_bundle.md),
[`print.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_fit.md),
[`print.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_psm.md),
[`summary.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_bundle.md),
[`summary.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_fit.md),
[`summary.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md),
[`summary.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_psm.md)
