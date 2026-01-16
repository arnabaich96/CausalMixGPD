# DPmixGPD

DPmixGPD delivers Dirichlet process mixtures with optional generalized
Pareto tails so you can model the bulk and extremes with shared mixture
weights and clean prediction APIs.

## Vignette Tour

### Getting Started

- **[Introduction](https://arnabaich96.github.io/DPmixGPD/articles/v01-introduction.md)** -
  Overview, three-phase workflow, PS strategies, and quickstart
- **[Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/v02-available-distributions.md)** -
  Comprehensive guide to all kernels plus GPD with dqrp functions
- **[Basic Model
  Workflow](https://arnabaich96.github.io/DPmixGPD/articles/v03-basic-model-compile-run.md)** -
  Detailed three-phase guide: specification -\> bundle -\> MCMC

### Unconditional Models (y only)

- **[DPmix with
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v04-unconditional-DPmix-CRP.md)** -
  Chinese Restaurant Process backend for bulk-only mixture modeling
- **[DPmix with
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v05-unconditional-DPmix-SB.md)** -
  Stick-Breaking backend comparison
- **[DPmixGPD with
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v06-unconditional-DPmixGPD-CRP.md)** -
  Adding GPD tail augmentation to CRP
- **[DPmixGPD with
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v07-unconditional-DPmixGPD-SB.md)** -
  SB backend with GPD tails

### Conditional Models (y \| X)

- **[Conditional DPmix
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v08-conditional-DPmix-CRP.md)** -
  CRP with covariates, heteroscedasticity analysis
- **[Conditional DPmix
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v09-conditional-DPmix-SB.md)** -
  SB with covariates
- **[Conditional DPmixGPD
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v10-conditional-DPmixGPD-CRP.md)** -
  CRP with covariates and GPD tail
- **[Conditional DPmixGPD
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v11-conditional-DPmixGPD-SB.md)** -
  SB with covariates and GPD tail

### Causal Inference (treatment effects)

- **[Same Backend
  CRP](https://arnabaich96.github.io/DPmixGPD/articles/v12-causal-same-backend-CRP.md)** -
  Causal inference with CRP for both arms (optional PS)
- **[Same Backend
  SB](https://arnabaich96.github.io/DPmixGPD/articles/v13-causal-same-backend-SB.md)** -
  Causal inference with SB for both arms (optional PS)
- **[Different Backends (CRP for
  PS)](https://arnabaich96.github.io/DPmixGPD/articles/v14-causal-different-backends-CRP.md)** -
  CRP for PS (optional), mixed outcome backends
- **[Different Backends (SB for
  PS)](https://arnabaich96.github.io/DPmixGPD/articles/v15-causal-different-backends-SB.md)** -
  SB for PS (optional), mixed outcome backends

## Available S3 Methods

DPmixGPD provides standard S3 methods for fitted model objects
(`mixgpd_fit` and `dpmixgpd_causal_fit`):

- [`summary()`](https://rdrr.io/r/base/summary.html) - Posterior
  summaries with ESS, Rhat, and diagnostics
- `params()` - Posterior parameter tables (bulk, tail, and thresholds)
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) - Trace and
  density plots via ggplot2 (causal returns treated/control lists)
- [`predict()`](https://rdrr.io/r/stats/predict.html) - Density, CDF,
  quantile, and survival predictions (causal: mean/quantile effects)
- [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) - Fitted
  values at observed data points

## Status checks

| Metric | Current value | Source |
|----|----|----|
| Test coverage (tests) | `0%` | `inst/extdata/coverage_status.json` (run `tools/update_coverage_status.R`) |
| Coverage helper | [`DPmixGPD::coverage_status()`](https://arnabaich96.github.io/DPmixGPD/reference/coverage_status.md) | runs [`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.md) and can refresh the JSON file |
| Status reader | [`DPmixGPD::read_coverage_status()`](https://arnabaich96.github.io/DPmixGPD/reference/read_coverage_status.md) | easy lookup for badges or pkgdown site |

Run `tools/update_coverage_status.R` after expanding the test suite to
keep the status data and home-page summary up to date.

## Kernel reference pages

- [Kernel:
  Normal](https://arnabaich96.github.io/DPmixGPD/articles/kernel-normal.md)
- [Kernel:
  Lognormal](https://arnabaich96.github.io/DPmixGPD/articles/kernel-lognormal.md)
- [Kernel:
  Gamma](https://arnabaich96.github.io/DPmixGPD/articles/kernel-gamma.md)
- [Kernel: Inverse
  Gaussian](https://arnabaich96.github.io/DPmixGPD/articles/kernel-invgauss.md)
- [Kernel:
  Laplace](https://arnabaich96.github.io/DPmixGPD/articles/kernel-laplace.md)
- [Kernel:
  Amoroso](https://arnabaich96.github.io/DPmixGPD/articles/kernel-amoroso.md)
- [Kernel:
  Cauchy](https://arnabaich96.github.io/DPmixGPD/articles/kernel-cauchy.md)

## Reference

- Function reference:
  [Reference](https://arnabaich96.github.io/DPmixGPD/reference/index.md)

## Project metadata

- License: [GPL-3
  License](https://github.com/arnabaich96/DPmixGPD_Package/blob/master/LICENSE)
- Contributing:
  [CONTRIBUTING.md](https://github.com/arnabaich96/DPmixGPD_Package/blob/master/CONTRIBUTING.md)
- Code of Conduct:
  [CODE_OF_CONDUCT.md](https://github.com/arnabaich96/DPmixGPD_Package/blob/master/CODE_OF_CONDUCT.md)
- Citation: Run `citation("DPmixGPD")` in R, or see
  [inst/CITATION](https://github.com/arnabaich96/DPmixGPD_Package/blob/master/inst/CITATION)
- Changelog:
  [NEWS](https://arnabaich96.github.io/DPmixGPD/news/index.md)
