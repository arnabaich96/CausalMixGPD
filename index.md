# DPmixGPD

DPmixGPD delivers Dirichlet process mixtures with optional generalized Pareto tails so you can model the bulk and extremes with shared mixture weights and clean prediction APIs.

## Vignette tour

- **Onboarding:** [Start here](articles/v00-start-here.html) explains what problems the package solves and presents the canonical simul + fit loop.
- **Installation & reproducibility:** [01-installation](articles/v01-installation-reproducibility.html) shows how to install, seed Nimble, and debug initialization warnings.
- **Single-outcome modeling:** [v02-single-outcome-modeling](articles/v02-single-outcome-modeling.html) tells the full story for bulk + tail fits.
- **Causal CQTE:** [v05-causal-cqte](articles/v05-causal-cqte.html) plus [v06-causal-extras](articles/v06-causal-extras.html) cover treatment-specific fits, CQTE plotting, and comparisons with ATE/CATE.
- **Backends:** [v03-backends-crp-vs-sb](articles/v03-backends-crp-vs-sb.html) contrasts the CRP and stick-breaking engines.
- **Kernel guidance:** [v04-kernels-guide](articles/v04-kernels-guide.html) and the kernel-specific pages provide parameter meanings, priors, and domain guidance.
- **Prediction & exports:** [v08-prediction-and-exports](articles/v08-prediction-and-exports.html) plus the legacy [Prediction](articles/prediction.html) vignette detail density, quantile, and survival forecasts.
- **Troubleshooting & survival:** [v09-troubleshooting](articles/v09-troubleshooting.html) and [v07-survival](articles/v07-survival.html) cover warning fixes and survival-style tips.
- **Developer resources:** [v10-developer-guide](articles/v10-developer-guide.html) walks through kernel registration, Nimble hooks, and the regression checklist.

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
