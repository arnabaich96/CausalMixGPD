# DPmixGPD

DPmixGPD fits **Dirichlet process mixture models** (CRP or stick-breaking backends) with an **optional Generalized Pareto (GPD) tail** so you can model the bulk and extremes *together* and still get clean, user-friendly prediction APIs.

## Install

```r
# install.packages("remotes")
remotes::install_github(
  "arnabaich96/DPmixGPD",
  build_vignettes = TRUE,
  INSTALL_opts = c("--html")
)
```

## Quickstart

```r
library(DPmixGPD)

set.seed(1)
y <- abs(rnorm(120)) + 0.2

bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel  = "lognormal",
  GPD     = TRUE,
  Kmax    = 6,
  mcmc = list(niter = 800, nburnin = 200, thin = 2, nchains = 1, seed = 1)
)

fit <- run_mcmc_bundle_manual(bundle)

summary(fit)
plot(fit, family = "trace")

# predictive quantities
predict(fit, type = "quantile", p = c(0.1, 0.5, 0.9))
```

## Learn the package by goal

### I want to

- **Get oriented fast** -> [Start Here](articles/v00-start-here.html)
- **Get oriented fast (overview)** -> [Introduction](articles/v01-introduction.html)
- **See every available distribution and its d/p/q/r helpers** -> [Available Distributions](articles/v02-available-distributions.html)
- **Understand the full workflow (spec -> bundle -> MCMC)** -> [Basic Model Workflow](articles/v05-basic-model-compile-run.html)
- **See how backends and kernels fit together** -> [Backends, Kernels, and Workflow Map](articles/v04-backends-and-workflow.html)

### Unconditional models (y only)

- [DPmix with CRP](articles/v06-unconditional-DPmix-CRP.html)
- [DPmix with SB](articles/v07-unconditional-DPmix-SB.html)
- [DPmixGPD with CRP](articles/v08-unconditional-DPmixGPD-CRP.html)
- [DPmixGPD with SB](articles/v09-unconditional-DPmixGPD-SB.html)

### Conditional models (y | X)

- [Conditional DPmix CRP](articles/v10-conditional-DPmix-CRP.html)
- [Conditional DPmix SB](articles/v11-conditional-DPmix-SB.html)
- [Conditional DPmixGPD CRP](articles/v12-conditional-DPmixGPD-CRP.html)
- [Conditional DPmixGPD SB](articles/v13-conditional-DPmixGPD-SB.html)

### Causal inference (two-arm outcome models)

- [No X (CRP)](articles/v14-causal-no-x-CRP.html)
- [X without PS (SB)](articles/v15-causal-x-no-ps-SB.html)
- [Same Backend CRP](articles/v16-causal-same-backend-CRP.html)
- [Same Backend SB](articles/v17-causal-same-backend-SB.html)
- [Different Backends (CRP)](articles/v18-causal-different-backends-CRP.html)
- [Different Backends (SB)](articles/v19-causal-different-backends-SB.html)

### Troubleshooting

- [Troubleshooting](articles/v20-troubleshooting.html)

## Kernel reference (short pages)

These pages are quick reminders of parameterizations and the relevant exported helpers.

- [Normal kernel](articles/kernel-normal.html)
- [Lognormal kernel](articles/kernel-lognormal.html)
- [Gamma kernel](articles/kernel-gamma.html)
- [Inverse Gaussian kernel](articles/kernel-invgauss.html)
- [Laplace kernel](articles/kernel-laplace.html)
- [Amoroso kernel](articles/kernel-amoroso.html)
- [Cauchy kernel](articles/kernel-cauchy.html)

## Core S3 methods

Fitted model objects (`mixgpd_fit` and `dpmixgpd_causal_fit`) support:

- `print()`, `summary()`
- `params()`
- `plot()`
- `predict()` (density / CDF / quantile / survival; causal: effects)
- `fitted()`

## Help and project links

- Function reference: [Reference](reference/index.html)
- Changelog: [News](news/index.html)
- Source + issues: GitHub (see the navbar link)

## Package health

| Metric | Current value | Source |
| --- | --- | --- |
| Test coverage (tests) | `0%` | `inst/extdata/coverage_status.json` (run `tools/update_coverage_status.R`) |
| Coverage helper | `DPmixGPD::coverage_status()` | runs `covr::package_coverage()` and can refresh the JSON file |
| Status reader | `DPmixGPD::read_coverage_status()` | easy lookup for badges or pkgdown site |
