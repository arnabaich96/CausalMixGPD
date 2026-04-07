# Predict labels or similarity matrices from a cluster fit

Convert posterior draws from a `dpmixgpd_cluster_fit` object into either
a representative clustering or a posterior similarity matrix (PSM). This
is the main post-processing step for the cluster workflow after
[`dpmix.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md)
or
[`dpmgpd.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.cluster.md).

## Usage

``` r
# S3 method for class 'dpmixgpd_cluster_fit'
predict(
  object,
  newdata = NULL,
  type = c("label", "psm"),
  burnin = NULL,
  thin = NULL,
  return_scores = FALSE,
  psm_max_n = 2000L,
  ...
)
```

## Arguments

- object:

  A fitted cluster object.

- newdata:

  Optional new data containing the response and predictors required by
  the original formula. New-data prediction is available only for
  `type = "label"`.

- type:

  Prediction target:

  - `"label"`: representative partition via Dahl's least-squares rule

  - `"psm"`: posterior similarity matrix on the training sample

- burnin:

  Number of initial posterior draws to discard.

- thin:

  Keep every `thin`-th posterior draw.

- return_scores:

  Logical; if `TRUE` and `type = "label"`, include the matrix of
  Dahl-cluster assignment scores.

- psm_max_n:

  Maximum training sample size allowed for `type = "psm"`.

- ...:

  Unused.

## Value

A `dpmixgpd_cluster_labels` object when `type = "label"` or a
`dpmixgpd_cluster_psm` object when `type = "psm"`.

## Details

Let \\z_i^{(s)}\\ denote the latent cluster label for observation \\i\\
at posterior draw \\s\\. The posterior similarity matrix is \$\$
\mathrm{PSM}\_{ij} = \Pr(z_i = z_j \mid y) \approx \frac{1}{S}
\sum\_{s=1}^S I(z_i^{(s)} = z_j^{(s)}). \$\$ The returned label solution
is the Dahl representative partition, obtained by choosing the draw
whose adjacency matrix is closest to the PSM in squared error.

For `newdata`, the function combines draw-specific component weights and
component densities to produce posterior assignment scores relative to
the representative training clusters. Returned `newdata` label objects
also carry the training labels and response data needed for comparative
`plot(..., type = "summary")` displays. A PSM is not defined for
`newdata`, so `type = "psm"` is restricted to the training sample.

Computing the PSM is \\O(n^2)\\ in the training sample size, so
`psm_max_n` guards against accidental large matrix allocations.

## See also

[`dpmix.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md),
[`dpmgpd.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.cluster.md),
[`summary.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_fit.md),
[`plot.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_fit.md),
[`summary.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md),
[`summary.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_psm.md).

Other cluster workflow:
[`dpmgpd.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.cluster.md),
[`dpmix.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md),
[`plot.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_bundle.md),
[`plot.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_fit.md),
[`plot.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_labels.md),
[`plot.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_psm.md),
[`print.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_bundle.md),
[`print.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_fit.md),
[`print.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_labels.md),
[`print.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.dpmixgpd_cluster_psm.md),
[`summary.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_bundle.md),
[`summary.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_fit.md),
[`summary.dpmixgpd_cluster_labels()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_labels.md),
[`summary.dpmixgpd_cluster_psm()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.dpmixgpd_cluster_psm.md)
