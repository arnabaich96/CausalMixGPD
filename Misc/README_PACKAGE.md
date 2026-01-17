# DPmixGPD

[![Codecov](https://codecov.io/gh/arnabaich96/DPmixGPD_Package/branch/master/graph/badge.svg)](https://codecov.io/gh/arnabaich96/DPmixGPD_Package)



DPmixGPD provides Dirichlet process mixtures with optional generalized Pareto tail augmentation to keep bulk and extreme modeling in sync. It supports both stick-breaking (SB) and Chinese Restaurant Process (CRP) backends plus a kernel registry so the same mixture weights control `predict()`, `quantile()`, `density()`, and causal CQTE outputs.

## Site Maintenance & QA

For documentation site quality assurance and maintenance processes, see this folder:
- [`DOCS_QA.md`](DOCS_QA.md) - Pre-release QA checklist
- [`SITE_POLISH_SUMMARY.md`](SITE_POLISH_SUMMARY.md) - Polish loop summary

Use `DOCS_QA.md` before each release to ensure the site stays polished and consistent.
