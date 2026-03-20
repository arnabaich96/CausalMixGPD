# Fit a clustering-only bulk-tail model

Variant of
[`dpmix.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md)
that augments the cluster kernel with a generalized Pareto tail. This is
the clustering analogue of the spliced bulk-tail workflow used by
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

## Usage

``` r
dpmgpd.cluster(
  formula,
  data,
  ...,
  type = c("weights", "param", "both"),
  default = "weights",
  mcmc = list()
)
```

## Arguments

- formula:

  Model formula. The response must be present in `data`.

- data:

  Data frame containing the response and optional predictors.

- ...:

  Additional arguments passed to `build_cluster_bundle()`, including
  kernel settings, prior overrides, component counts, and monitoring
  controls.

- type:

  Clustering mode. `"weights"` links mixture weights to predictors,
  `"param"` links kernel parameters to predictors, and `"both"` does
  both.

- default:

  Default mode used when `type` is omitted.

- mcmc:

  MCMC control list passed into the cluster bundle.

## Value

Object of class `dpmixgpd_cluster_fit`.

## Details

For observations above a component-specific threshold, the component
density is spliced as \$\$ f(y) = (1 - F\_{bulk}(u)) g\_{GPD}(y \mid u,
\sigma_u, \xi_u), \qquad y \ge u, \$\$ so cluster assignment can be
informed by both central behavior and tail behavior.

This interface is preferable when cluster separation is driven by
upper-tail differences rather than bulk-only shape or location
differences.

## See also

[`dpmix.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md),
[`predict.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.dpmixgpd_cluster_fit.md),
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md),
[`sim_bulk_tail()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/sim_bulk_tail.md).

Other cluster workflow:
[`dpmix.cluster()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmix.cluster.md),
[`plot.dpmixgpd_cluster_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_bundle.md),
[`plot.dpmixgpd_cluster_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.dpmixgpd_cluster_fit.md),
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
