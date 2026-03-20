# CausalMixGPD

**CausalMixGPD** implements Dirichlet process mixture models (CRP or
stick-breaking) with an optional generalized Pareto tail and a
consistent prediction API.

## Installation

``` r

# install.packages("remotes")
remotes::install_github(
  "arnabaich96/CausalMixGPD",
  build_vignettes = TRUE,
  INSTALL_opts = c("--html")
)
```

## Reference

- [**Function
  reference**](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/index.html)

## Additional docs

- [**Package
  roadmap**](https://arnabaich96.github.io/CausalMixGPD/start/roadmap.html)

## Performance Acceptance Workflow

- Acceptance gate tests: `tests/testthat/test-performance-acceptance.R`
- Checklist-to-test mapping: `tests/perf/acceptance_checklist_map.md`
- Benchmark script: `tests/perf/benchmark_acceptance.R`
- Required benchmark artifact:
  `tests/perf/benchmark_acceptance_report.md`
- Phase 2 benchmark script: `tests/perf/benchmark_phase2.R`
- Phase 2 benchmark artifact: `tests/perf/benchmark_phase2_report.md`

Use `DPMIXGPD_TEST_LEVEL=ci` for CI-level acceptance checks and
`DPMIXGPD_TEST_LEVEL=full` for full seeded-equivalence runs.
