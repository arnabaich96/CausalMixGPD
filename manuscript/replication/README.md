# CausalMixGPD Replication

This folder contains standalone replication scripts for `CausalMixGPD`, an R package for Bayesian analysis of heavy-tailed outcomes using Dirichlet process mixture models for the body of the distribution with optional generalized Pareto tails. The package supports unconditional and covariate-modulated mixtures, uses `nimble` for MCMC estimation, and extends to mixtures of different treatment-arm outcomes for causal inference. Posterior summaries include densities, quantiles, expected values, survival functions, and causal effects, with emphasis on tail-sensitive functionals and extreme quantiles.

## Package Metadata

- Title: Dirichlet Process Mixtures with Generalized Pareto Tail
- Version: 0.6.0
- Author/Maintainer: Arnab Aich (<aaich@fsu.edu>)
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
install.packages(c(
  "CausalMixGPD",
  "MASS",
  "MatchIt",
  "ggplot2",
  "patchwork"
))
```

`nimble` loads through `CausalMixGPD`. Windows users may need Rtools for compilation.

## Run

Run any script from this folder with `Rscript`:

```sh
Rscript Rscripts/overview_onearm.R
Rscript Rscripts/overview_clustering.R
Rscript Rscripts/overview_causal.R
Rscript Rscripts/data_analysis_cluster.R
Rscript Rscripts/data_analysis_causal.R
```

## Links

- GitHub repository: https://github.com/arnabaich96/CausalMixGPD
- Package website: https://arnabaich96.github.io/CausalMixGPD/
- Function reference: https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/index.html
- Articles index: https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/index.html
- JSS article page: https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/CausalMixGPD_JSS_article.html

## Replication Script Mappings

- `Rscripts/overview_onearm.R`: https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/cmgpd_one_arm.html
- `Rscripts/overview_clustering.R`: https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/cmgpd_clustering.html
- `Rscripts/overview_causal.R`: https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/cmgpd_causal.html
- `Rscripts/data_analysis_cluster.R`: https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/CausalMixGPD_JSS_article.html
- `Rscripts/data_analysis_causal.R`: https://arnabaich96.github.io/CausalMixGPD/pkgdown/articles/CausalMixGPD_JSS_article.html

## Dependency Links

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
