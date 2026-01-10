# DPmixGPD

DPmixGPD delivers Dirichlet process mixtures with optional generalized
Pareto tails so you can model the bulk and extremes with shared mixture
weights and clean prediction APIs.

## Vignette tour

- **Onboarding:** [Start
  here](https://example.com/DPmixGPD/articles/v00-start-here.md)
  explains what problems the package solves and presents the canonical
  simul + fit loop.
- **Installation & reproducibility:**
  [01-installation](https://example.com/DPmixGPD/articles/v01-installation-reproducibility.md)
  shows how to install, seed Nimble, and debug initialization warnings.
- **Single-outcome modeling:**
  [v02-single-outcome-modeling](https://example.com/DPmixGPD/articles/v02-single-outcome-modeling.md)
  tells the full story for bulk + tail fits.
- **Causal CQTE:**
  [v05-causal-cqte](https://example.com/DPmixGPD/articles/v05-causal-cqte.md)
  plus
  [v06-causal-extras](https://example.com/DPmixGPD/articles/v06-causal-extras.md)
  cover treatment-specific fits, CQTE plotting, and comparisons with
  ATE/CATE.
- **Backends:**
  [v03-backends-crp-vs-sb](https://example.com/DPmixGPD/articles/v03-backends-crp-vs-sb.md)
  contrasts the CRP and stick-breaking engines.
- **Kernel guidance:**
  [v04-kernels-guide](https://example.com/DPmixGPD/articles/v04-kernels-guide.md)
  and the kernel-specific pages provide parameter meanings, priors, and
  domain guidance.
- **Prediction & exports:**
  [v08-prediction-and-exports](https://example.com/DPmixGPD/articles/v08-prediction-and-exports.md)
  plus the legacy
  [Prediction](https://example.com/DPmixGPD/articles/prediction.md)
  vignette detail density, quantile, and survival forecasts.
- **Troubleshooting & survival:**
  [v09-troubleshooting](https://example.com/DPmixGPD/articles/v09-troubleshooting.md)
  and
  [v07-survival](https://example.com/DPmixGPD/articles/v07-survival.md)
  cover warning fixes and survival-style tips.
- **Developer resources:**
  [v10-developer-guide](https://example.com/DPmixGPD/articles/v10-developer-guide.md)
  walks through kernel registration, Nimble hooks, and the regression
  checklist.

## Status checks

| Metric | Current value | Source |
|----|----|----|
| Test coverage (tests) | `0%` | `inst/extdata/coverage_status.json` (run `tools/update_coverage_status.R`) |
| Coverage helper | [`DPmixGPD::coverage_status()`](https://example.com/DPmixGPD/reference/coverage_status.md) | runs [`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.md) and can refresh the JSON file |
| Status reader | [`DPmixGPD::read_coverage_status()`](https://example.com/DPmixGPD/reference/read_coverage_status.md) | easy lookup for badges or pkgdown site |

Run `tools/update_coverage_status.R` after expanding the test suite to
keep the status data and home-page summary up to date.

## Kernel reference pages

- [Kernel:
  Normal](https://example.com/DPmixGPD/articles/kernel-normal.md)
- [Kernel:
  Lognormal](https://example.com/DPmixGPD/articles/kernel-lognormal.md)
- [Kernel: Gamma](https://example.com/DPmixGPD/articles/kernel-gamma.md)
- [Kernel: Inverse
  Gaussian](https://example.com/DPmixGPD/articles/kernel-invgauss.md)
- [Kernel:
  Laplace](https://example.com/DPmixGPD/articles/kernel-laplace.md)
- [Kernel:
  Amoroso](https://example.com/DPmixGPD/articles/kernel-amoroso.md)
- [Kernel:
  Cauchy](https://example.com/DPmixGPD/articles/kernel-cauchy.md)

## Reference

- Function reference:
  [Reference](https://example.com/DPmixGPD/reference/index.md)
