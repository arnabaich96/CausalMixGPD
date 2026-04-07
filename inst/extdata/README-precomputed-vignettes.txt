This folder contains precomputed artifacts used when building package vignettes.

The vignettes avoid storing full fitted MCMC objects because those can be very
large and unstable for package distribution and checks. Instead, we ship only
lightweight outputs (prediction summaries, effect objects, small tables, and
static diagnostic figures).

1) Lightweight vignette output bundles
   - one_arm_outputs.rds      — prediction/summary outputs for vignettes/cmgpd_one_arm.Rmd
   - causal_outputs.rds       — effect objects + prediction outputs for vignettes/cmgpd_causal.Rmd
   - clustering_outputs.rds   — PSM/label objects + profile tables for vignettes/cmgpd_clustering.Rmd

   Supporting static diagnostics (examples):
   - one_arm_fit_summary.txt
   - one_arm_alpha_trace.png
   - one_arm_alpha_density.png
   - causal_fit_summary.txt
   - clustering_fit_summary.txt

   Regenerate artifacts with:
     Rscript data-raw/build_vignette_fits.R
   from the package root (requires pkgload, nimble, and full MCMC dependencies).

2) Legacy CSV / figure exports (older static tables)
   - unconditional_quantiles.csv, conditional_quantiles.csv
   - causal_ate.csv, causal_qte.csv
   - causal_qte.png (if present)

Prebuilt vignette HTML may also be shipped under inst/doc/ where applicable.

End users do not need to regenerate these files to read the vignettes.
