# CausalMixGPD Replication Material

Run `replicate.R` as the single entrypoint for package-level replication:

```r
source(system.file("replication", "replicate.R", package = "CausalMixGPD"))
```

Or from a source checkout:

```sh
Rscript inst/replication/replicate.R ./replication-output
```

The script uses public package APIs only and writes compact tables and figures
for the one-arm, clustering, and causal workflows. Increase the MCMC settings in
the script when producing final manuscript artifacts.
