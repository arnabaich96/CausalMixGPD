# DPmixGPD Global Contracts (Frozen)

This file records the non-negotiable modeling rules for DPmixGPD. Treat this as
the single source of truth for what can and cannot be demonstrated or tested.

## Models
- Backends: CRP and SB only.
- Kernels (7 total):
  - Positive support: gamma, lognormal, invgauss, amoroso (amoroso shown with shape1 = 1 in vignettes).
  - Real line: normal, laplace, cauchy.

## GPD rules
- GPD is either TRUE or FALSE.
- Cauchy is never paired with GPD.
- GPD is shown only on datasets that were generated with explicit tail exceedances.

## Mixture truth
- All datasets are generated from finite mixtures with K in {2, 3, 4}.
- Never use K < 2 or K > 4; spread K across datasets (K=2,3,4 all exercised).

## Vignette runtime rules
- Always print head() of each dataset plus a meta line (n/N, support, p, K_true, tail flag).
- MCMC settings: >= 1000 iterations, 250 burn-in, >= 2 chains (up to 3).
- Prediction examples: use >= 20% newdata and report >= 4 quantiles.
