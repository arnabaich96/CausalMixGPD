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

The manuscript directory contains longer build helpers for regenerating article
figures, but those helpers are not separate replication entrypoints.

## Output Map

The script writes a `manifest.csv` file mapping each generated artifact to the
manuscript workflow it supports. The compact reviewer outputs are:

- `one_arm_summary.csv`, `one_arm_quantiles.csv`, `one_arm_quantiles.png`: one-arm package overview.
- `cluster_profiles.csv`, `cluster_sizes.png`: clustering workflow and public `cluster_profiles()` accessor.
- `causal_ate.csv`, `causal_ate.png`: causal workflow and ATE effect display.
- `session-info.txt`: package version, seed, output directory, and `sessionInfo()`.

These outputs are intentionally small so reviewers can verify the workflow
quickly. The manuscript build helpers use the same public APIs with longer MCMC
settings to regenerate full article figures and tables.
