This folder contains small, precomputed artifacts (tables and figures) that the
CRAN vignettes display.

Prebuilt vignette HTML pages are shipped separately under `inst/doc/` so source
installs still provide readable vignette content even when local vignette
building is skipped.

Rationale
---------
The DPmixGPD vignettes demonstrate full model workflows, but running MCMC during
CRAN checks is discouraged (time variability and long runtimes). Instead, we
precompute lightweight outputs locally and ship them as static files.

How to regenerate
-----------------
These files are generated during package development and are shipped with the
package for vignette rendering. End users do not need to regenerate them.

Files written by the script
---------------------------
  - unconditional_quantiles.csv
  - unconditional_density.png
  - conditional_quantiles.csv
  - conditional_density.png
  - causal_qte.csv
  - causal_ate.csv
  - causal_qte.png
