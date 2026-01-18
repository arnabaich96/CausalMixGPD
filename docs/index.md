# DPmixGPD

DPmixGPD fits **Dirichlet process mixture models** (CRP or
stick-breaking backends) with an **optional Generalized Pareto (GPD)
tail** so you can model the bulk and extremes *together* and still get
clean, user-friendly prediction APIs.

## Install

``` r
# install.packages("remotes")
remotes::install_github("arnabaich96/DPmixGPD")
```

## Quickstart

``` r
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

- **Get oriented fast**
  [Start Here](https://arnabaich96.github.io/DPmixGPD/articles/v00-start-here.html)
- **Get oriented fast (overview)**
  [Introduction](https://arnabaich96.github.io/DPmixGPD/articles/v01-introduction.html)
- **See every available distribution and its d/p/q/r helpers**
  [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/v02-available-distributions.html)
- **Understand the full workflow (spec ? bundle ? MCMC)** [Basic Model
  Workflow](https://arnabaich96.github.io/DPmixGPD/articles/v05-basic-model-compile-run.html)
- **See how backends and kernels fit together** [Backends, Kernels, and
  Workflow Map](https://arnabaich96.github.io/DPmixGPD/articles/v04-backends-and-workflow.html)

### Unconditional models (y only)

- [DPmix with
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v06-unconditional-DPmix-CRP.html)
- [DPmix with
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v07-unconditional-DPmix-SB.html)
- [DPmixGPD with
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v08-unconditional-DPmixGPD-CRP.html)
- [DPmixGPD with
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v09-unconditional-DPmixGPD-SB.html)

### Conditional models (y \| X)

- [Conditional DPmix
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v10-conditional-DPmix-CRP.html)
- [Conditional DPmix
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v11-conditional-DPmix-SB.html)
- [Conditional DPmixGPD
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v12-conditional-DPmixGPD-CRP.html)
- [Conditional DPmixGPD
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v13-conditional-DPmixGPD-SB.html)

### Causal inference (two-arm outcome models)

- [No X (CRP)](https://arnabaich96.github.io/DPmixGPD/articles/v14-causal-no-x-CRP.html)
- [X without PS (SB)](https://arnabaich96.github.io/DPmixGPD/articles/v15-causal-x-no-ps-SB.html)
- [Same Backend
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v16-causal-same-backend-CRP.html)
- [Same Backend
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v17-causal-same-backend-SB.html)
- [Different Backends (CRP for
  PS)](https://arnabaich96.github.io/DPmixGPD/articles/v18-causal-different-backends-CRP.html)
- [Different Backends (SB for
  PS)](https://arnabaich96.github.io/DPmixGPD/articles/v19-causal-different-backends-SB.html)

### Troubleshooting

- [Troubleshooting](https://arnabaich96.github.io/DPmixGPD/articles/v20-troubleshooting.html)

## Kernel reference (short pages)

These pages are quick reminders of parameterizations and the relevant
exported helpers.

- [Normal
  kernel](https://arnabaich96.github.io/DPmixGPD/articles/kernel-normal.html)
- [Lognormal
  kernel](https://arnabaich96.github.io/DPmixGPD/articles/kernel-lognormal.html)
- [Gamma
  kernel](https://arnabaich96.github.io/DPmixGPD/articles/kernel-gamma.html)
- [Inverse Gaussian
  kernel](https://arnabaich96.github.io/DPmixGPD/articles/kernel-invgauss.html)
- [Laplace
  kernel](https://arnabaich96.github.io/DPmixGPD/articles/kernel-laplace.html)
- [Amoroso
  kernel](https://arnabaich96.github.io/DPmixGPD/articles/kernel-amoroso.html)
- [Cauchy
  kernel](https://arnabaich96.github.io/DPmixGPD/articles/kernel-cauchy.html)

## Core S3 methods

Fitted model objects (`mixgpd_fit` and `dpmixgpd_causal_fit`) support:

- [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html)html
- [`params()`](https://arnabaich96.github.io/DPmixGPD/reference/params.md)
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
- [`predict()`](https://rdrr.io/r/stats/predict.html) (density / CDF /
  quantile / survival; causal: effects)
- [`fitted()`](https://rdrr.io/r/stats/fitted.values.html)

## Help and project links

- Function reference:
  [Reference](https://arnabaich96.github.io/DPmixGPD/reference/index.html)
- Changelog:
  [News](https://arnabaich96.github.io/DPmixGPD/news/index.html)
- Source + issues: GitHub (see the navbar link)

## Package health

| Metric | Current value | Source |
|----|----|----|
| Test coverage (tests) | `0%` | `inst/extdata/coverage_status.json` (run `tools/update_coverage_status.R`) |
| Coverage helper | [`DPmixGPD::coverage_status()`](https://arnabaich96.github.io/DPmixGPD/reference/coverage_status.html) | runs [`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.html) and can refresh the JSON file |
| Status reader | [`DPmixGPD::read_coverage_status()`](https://arnabaich96.github.io/DPmixGPD/reference/read_coverage_status.html) | easy lookup for badges or pkgdown site |

