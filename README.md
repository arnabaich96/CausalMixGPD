# CausalMixGPD

**CausalMixGPD** provides Dirichlet process mixture modeling (CRP, stick-breaking, and spliced variants) with optional generalized Pareto tails and a unified prediction/causal inference API.

Current package title: **Bayesian Nonparametric Conditional Density Modeling in Causal Inference and Clustering with a Heavy-Tail Extension**.

Current package version: see the `Version` field in `DESCRIPTION`.

CRAN package DOI: `10.32614/CRAN.package.CausalMixGPD`.

Methodology paper by Aich and Bhattacharya: <https://doi.org/10.5281/zenodo.19672760>.

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
- CRAN package DOI: `10.32614/CRAN.package.CausalMixGPD`
- [**Aich-Bhattacharya methodology paper**](https://doi.org/10.5281/zenodo.19672760)

## Citation

Use `citation("CausalMixGPD")` and report the exact package version used. Cite the CRAN package DOI for the R package, and cite the Aich-Bhattacharya methodology paper DOI for the statistical method.

## Additional Docs

- [**Package roadmap**](https://arnabaich96.github.io/CausalMixGPD/start/roadmap.html)

## Validation Notes

- Performance acceptance tests and benchmark scripts are under `tests/perf/`.
- Main acceptance test entrypoint: `tests/testthat/test-performance-acceptance.R`.

Use `DPMIXGPD_TEST_LEVEL=ci` for CI-level acceptance checks and
`DPMIXGPD_TEST_LEVEL=full` for full seeded-equivalence runs.



