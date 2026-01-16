# DPmixGPD

DPmixGPD delivers Dirichlet process mixtures with optional generalized Pareto tails so you can model the bulk and extremes with shared mixture weights and clean prediction APIs.

## Vignette Tour

### Getting Started

- **[Introduction](articles/v01-introduction.html)** - Overview, three-phase workflow, PS strategies, and quickstart
- **[Available Distributions](articles/v02-available-distributions.html)** - Comprehensive guide to all kernels plus GPD with dqrp functions
- **[Basic Model Workflow](articles/v03-basic-model-compile-run.html)** - Detailed three-phase guide: specification -> bundle -> MCMC

### Unconditional Models (y only)

- **[DPmix with CRP](articles/v04-unconditional-DPmix-CRP.html)** - Chinese Restaurant Process backend for bulk-only mixture modeling
- **[DPmix with SB](articles/v05-unconditional-DPmix-SB.html)** - Stick-Breaking backend comparison
- **[DPmixGPD with CRP](articles/v06-unconditional-DPmixGPD-CRP.html)** - Adding GPD tail augmentation to CRP
- **[DPmixGPD with SB](articles/v07-unconditional-DPmixGPD-SB.html)** - SB backend with GPD tails

### Conditional Models (y | X)

- **[Conditional DPmix CRP](articles/v08-conditional-DPmix-CRP.html)** - CRP with covariates, heteroscedasticity analysis
- **[Conditional DPmix SB](articles/v09-conditional-DPmix-SB.html)** - SB with covariates
- **[Conditional DPmixGPD CRP](articles/v10-conditional-DPmixGPD-CRP.html)** - CRP with covariates and GPD tail
- **[Conditional DPmixGPD SB](articles/v11-conditional-DPmixGPD-SB.html)** - SB with covariates and GPD tail

### Causal Inference (treatment effects)

- **[Same Backend CRP](articles/v12-causal-same-backend-CRP.html)** - Causal inference with CRP for both arms (optional PS)
- **[Same Backend SB](articles/v13-causal-same-backend-SB.html)** - Causal inference with SB for both arms (optional PS)
- **[Different Backends (CRP for PS)](articles/v14-causal-different-backends-CRP.html)** - CRP for PS (optional), mixed outcome backends
- **[Different Backends (SB for PS)](articles/v15-causal-different-backends-SB.html)** - SB for PS (optional), mixed outcome backends

## Available S3 Methods

DPmixGPD provides standard S3 methods for fitted model objects (`mixgpd_fit` and `dpmixgpd_causal_fit`):

- `summary()` - Posterior summaries with ESS, Rhat, and diagnostics
- `params()` - Posterior parameter tables (bulk, tail, and thresholds)
- `plot()` - Trace and density plots via ggplot2 (causal returns treated/control lists)
- `predict()` - Density, CDF, quantile, and survival predictions (causal: mean/quantile effects)
- `fitted()` - Fitted values at observed data points

## Status checks

| Metric | Current value | Source |
| --- | --- | --- |
| Test coverage (tests) | `0%` | `inst/extdata/coverage_status.json` (run `tools/update_coverage_status.R`) |
| Coverage helper | `DPmixGPD::coverage_status()` | runs `covr::package_coverage()` and can refresh the JSON file |
| Status reader | `DPmixGPD::read_coverage_status()` | easy lookup for badges or pkgdown site |

Run `tools/update_coverage_status.R` after expanding the test suite to keep the status data and home-page summary up to date.

## Kernel reference pages

- [Kernel: Normal](articles/kernel-normal.html)
- [Kernel: Lognormal](articles/kernel-lognormal.html)
- [Kernel: Gamma](articles/kernel-gamma.html)
- [Kernel: Inverse Gaussian](articles/kernel-invgauss.html)
- [Kernel: Laplace](articles/kernel-laplace.html)
- [Kernel: Amoroso](articles/kernel-amoroso.html)
- [Kernel: Cauchy](articles/kernel-cauchy.html)

## Reference

- Function reference: [Reference](reference/index.html)
