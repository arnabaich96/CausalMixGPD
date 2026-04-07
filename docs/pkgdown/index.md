# CausalMixGPD

`CausalMixGPD` provides Bayesian Dirichlet process mixture modeling for
heavy-tailed outcomes, with optional generalized Pareto tails and
extensions for causal inference and predictor-dependent clustering.

## Start Here

The package documentation is organized around three main workflows:

- [One-arm
  modeling](https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/cmgpd_one_arm.md)
- [Causal
  inference](https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/cmgpd_causal.md)
- [Predictor-dependent
  clustering](https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/cmgpd_clustering.md)

## Reference

- [Function
  reference](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/index.md)

## Installation

``` r

# install.packages("remotes")
remotes::install_github(
  "arnabaich96/CausalMixGPD",
  build_vignettes = TRUE,
  INSTALL_opts = c("--html")
)
```

## Notes

- The article pages above are the best entry points for model setup,
  interpretation, and worked examples.
- The reference section is useful once you know which workflow and
  function family you need.
