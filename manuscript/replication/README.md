# CausalMixGPD Manuscript Guide

This directory contains manuscript build notes for CausalMixGPD. The submitted
package-level replication material is the single script
`inst/replication/replicate.R`; use that entrypoint for reviewer replication.
The longer manuscript analysis scripts are build helpers for regenerating full
figures and tables with larger MCMC settings.

Reviewer replication should start with the package-level script, which writes a
`manifest.csv` output map and `session-info.txt`. The build helpers in this
directory are not independent replication entrypoints; they exist only to
regenerate the longer manuscript-specific figures after the package workflow has
been verified.

The additional clustering extension allows for supervised clustering with predictor-dependent mixture weights. Since only one API is provided for making predictions and conducting causal inference, the weights for the predictive and causal components must be the same.

## Package Metadata

- Title: An R Package for Bayesian Nonparametric Conditional Density Modeling in Causal Inference and Clustering with a Heavy-Tail Extension
- Version: see the `Version` field in `../../DESCRIPTION`
- Author/Maintainer: Arnab Aich
- License: GPL-3
- Depends: R (>= 4.0.0)
- Imports: `nimble`

## Manuscript Build Helpers

- `build_helpers/overview_onearm.R`: package overview for one-arm modeling and prediction.
- `build_helpers/overview_clustering.R`: package overview for clustering.
- `build_helpers/overview_causal.R`: package overview for causal modeling and prediction.
- `build_helpers/data_analysis_cluster.R`: Boston housing clustering analysis.
- `build_helpers/data_analysis_causal.R`: Lalonde causal analysis with printed profile summaries.

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
- CRAN package DOI: 10.32614/CRAN.package.CausalMixGPD

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
