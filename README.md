# DPmixGPD

[![Codecov](https://codecov.io/gh/arnabaich96/DPmixGPD_Package/branch/master/graph/badge.svg)](https://codecov.io/gh/arnabaich96/DPmixGPD_Package)

DPmixGPD provides Dirichlet process mixtures with optional generalized Pareto tail augmentation to keep bulk and extreme modeling in sync. It supports both stick-breaking (SB) and Chinese Restaurant Process (CRP) backends plus a kernel registry so the same mixture weights control `predict()`, `quantile()`, `density()`, and causal CQTE outputs.

## Download & install

1. **CRAN release** (best for stability):

```r
install.packages("DPmixGPD")
```

2. **Development preview** (GitHub placeholder):

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("arnabaich96/DPmixGPD")
```

## Quick start snapshot

```r
library(DPmixGPD)
y <- sim_bulk_tail(n = 120, seed = 1)
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "lognormal",
  GPD = TRUE,
  J = 6,
  mcmc = list(niter = 1200, nburnin = 400, thin = 2, nchains = 2, seed = c(1, 2))
)
fit <- run_mcmc_bundle_manual(bundle)
print(fit)
predict(fit, type = "quantile", probs = c(0.5, 0.9, 0.99))
```

## Documentation & vignettes

- Home: [GitHub repository](https://github.com/arnabaich96/DPmixGPD)
- Pkgdown site: https://arnabaich96.github.io/DPmixGPD/ (when published)
- Vignette tour: see the `articles/` list on the website (Start here, Single-outcome modeling, Causal CQTE, Backends, Kernels, Prediction, Troubleshooting, Developer guide).
- Kernel registry: follow the kernel-specific pages for parameter meanings and priors.
- Reference: `?build_nimble_bundle`, `?predict.mixgpd_fit`, `?cqte`.

## Notes

- Each kernel's mixture index is denoted by `j`, and the components argument is `J`.
- `GPD = TRUE/FALSE` controls whether the tail module is active; prediction reuses the sampling weights from the same run so diagnostics stay aligned.

## Status & coverage

| Metric | Value | How to refresh |
| --- | --- | --- |
| Test coverage (latest snapshot) | `0%` (tests) | `Rscript tools/update_coverage_status.R` |
| Status reader | `coverage_status()` / `read_coverage_status()` | Call from any R script to capture or read the JSON file at `inst/extdata/coverage_status.json`. |

The `tools/update_coverage_status.R` script runs `covr::package_coverage()` (via `coverage_status()` in `R/status.R`) and persists the JSON that feeds pkgdown/index summaries.

## Kernel support matrix

| Kernel | Has GPD tail | Accepts covariates (X) | SB backend | CRP backend |
| --- | --- | --- | --- | --- |
| normal | ✔ | ✔ | ✔ | ✔ |
| lognormal | ✔ | ✔ | ✔ | ✔ |
| invgauss | ✔ | ✔ | ✔ | ✔ |
| gamma | ✔ | ✔ | ✔ | ✔ |
| laplace | ✔ | ✔ | ✔ | ✔ |
| amoroso | ✔ | ✔ | ✔ | ✔ |
| cauchy | ❌ | ✔ | ✔ | ✔ |

Run `Rscript tools/kernel_support_table.R` to regenerate the markdown snippet above whenever the kernel registry changes; it relies on `kernel_support_table()` to reflect the registry definitions tested in `tests/testthat/test-kernels.R`.

---

## 📚 Site Maintenance & QA

For documentation site quality assurance and maintenance processes, see **[DOCS-MAINTENANCE/](DOCS-MAINTENANCE/)** folder:
- [`DOCS-MAINTENANCE/DOCS_QA.md`](DOCS-MAINTENANCE/DOCS_QA.md) — Pre-release QA checklist
- [`DOCS-MAINTENANCE/SITE_POLISH_SUMMARY.md`](DOCS-MAINTENANCE/SITE_POLISH_SUMMARY.md) — Polish loop summary

Use `DOCS-MAINTENANCE/DOCS_QA.md` before each release to ensure the site stays polished and consistent.
