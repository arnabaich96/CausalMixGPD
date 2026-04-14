# CausalMixGPD Manuscript Guide

This directory contains the scripts used to replicate CausalMixGPD manuscript, which is an R package for performing Bayesian modeling of heavy-tailed data. In the model implemented in the package, the core component follows a Dirichlet process mixture, optionally paired with generalized Pareto tails. Unconditional mixtures, conditional mixtures based on covariates, nimble MCMC sampling, and pooling data across treatment arms for causal inference can be included within the package. As output, posterior estimates of probability density functions, quantiles, means, survival functions, and causal effects are generated with a particular emphasis on tail-based statistics and extreme quantiles.

The additional clustering extension allows for supervised clustering with predictor-dependent mixture weights. Since only one API is provided for making predictions and conducting causal inference, the weights for the predictive and causal components must be the same.

## Package Metadata

- Title: An R Package for Bayesian Nonparametric Conditional Density Modeling in Causal Inference and Clustering with a Heavy-Tail Extension
- Version: 0.6.0
- Author/Maintainer: Arnab Aich
- License: GPL-3
- Depends: R (>= 4.0.0), `nimble`

## Replication Scripts

- `Rscripts/overview_onearm.R`: package overview for one-arm modeling and prediction.
- `Rscripts/overview_clustering.R`: package overview for clustering.
- `Rscripts/overview_causal.R`: package overview for causal modeling and prediction.
- `Rscripts/data_analysis_cluster.R`: Boston housing clustering analysis.
- `Rscripts/data_analysis_causal.R`: Lalonde causal analysis with printed profile summaries.

## Installation

Install the packages needed to run the standalone scripts:

```r
install.packages(c("CausalMixGPD","MASS","MatchIt","ggplot2","patchwork"))
```

`nimble` loads through `CausalMixGPD`. Windows users may need Rtools for compilation.


## Useful Links

- GitHub repository: https://github.com/arnabaich96/CausalMixGPD
- Package website: https://arnabaich96.github.io/CausalMixGPD/
- Function reference: https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/index.html

## Dependencies

- `nimble`: https://cran.r-project.org/package=nimble
- `MASS`: https://cran.r-project.org/package=MASS
- `MatchIt`: https://cran.r-project.org/package=MatchIt
- `ggplot2`: https://cran.r-project.org/package=ggplot2
- `patchwork`: https://cran.r-project.org/package=patchwork
- `future`: https://cran.r-project.org/package=future
- `future.apply`: https://cran.r-project.org/package=future.apply

## Data References

- Boston housing data (`MASS::Boston`): https://www.rdocumentation.org/packages/MASS/topics/Boston
- Lalonde data (`MatchIt::lalonde`): https://kosukeimai.github.io/MatchIt/reference/lalonde.html
