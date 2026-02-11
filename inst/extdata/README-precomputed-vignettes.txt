This folder contains small, precomputed artifacts (tables and figures) that the
CRAN vignettes display.

Rationale
---------
The DPmixGPD vignettes demonstrate full model workflows, but running MCMC during
CRAN checks is discouraged (time variability and long runtimes). Instead, we
precompute lightweight outputs locally and ship them as static files.

How to regenerate
-----------------
From the package root, run:

  source("tools/make_vignette_artifacts.R")

This script writes the files listed below into inst/extdata/. Rebuild vignettes
and run R CMD check before release.

Files written by the script
---------------------------
  - unconditional_quantiles.csv
  - unconditional_density.png
  - conditional_quantiles.csv
  - conditional_density.png
  - causal_qte.csv
  - causal_ate.csv
  - causal_qte.png