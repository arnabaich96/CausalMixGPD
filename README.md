# CausalMixGPD

**CausalMixGPD** provides Dirichlet process mixture modeling (CRP, stick-breaking, and spliced variants) with optional generalized Pareto tails and a unified prediction/causal inference API.

## Installation

```r
# install.packages("remotes")
remotes::install_github(
  "arnabaich96/CausalMixGPD",
  build_vignettes = TRUE,
  INSTALL_opts = c("--html")
)
```

## Recent Changes (2026-04)

- Added support for lognormal `threshold` link-distributions in GPD workflows, including spliced backend behavior.
- Improved initialization for stability with covariate-aware threshold/link seeding and stronger latent label starts.
- Added CRP retry logic for rare all-`-Inf` initialization failures during MCMC startup.
- Updated wrappers/tests/manuscript examples to align with the new threshold-link and initialization behavior.

## Documentation

- [**Function reference**](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/index.html)

## Additional Docs

- [**Package roadmap**](https://arnabaich96.github.io/CausalMixGPD/start/roadmap.html)

## Validation Notes

- Performance acceptance tests and benchmark scripts are under `tests/perf/`.
- Main acceptance test entrypoint: `tests/testthat/test-performance-acceptance.R`.

Use `DPMIXGPD_TEST_LEVEL=ci` for CI-level acceptance checks and
`DPMIXGPD_TEST_LEVEL=full` for full seeded-equivalence runs.



