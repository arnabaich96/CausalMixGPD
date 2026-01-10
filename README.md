# DPmixGPD

DPmixGPD provides Dirichlet process mixtures with optional generalized Pareto tail augmentation to keep bulk and extreme modeling in sync. It supports both stick-breaking (SB) and Chinese Restaurant Process (CRP) backends plus a kernel registry so the same mixture weights control `predict()`, `quantile()`, `density()`, and causal CQTE outputs.

## Download & install

1. **CRAN release** (best for stability):

```r
install.packages("DPmixGPD")
```

2. **Development preview** (GitHub placeholder):

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("$LINK")
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

- Home: [DPmixGPD pkgdown site](https://example.com/DPmixGPD)
- Vignette tour: see the `articles/` list on the website (Start here, Single-outcome modeling, Causal CQTE, Backends, Kernels, Prediction, Troubleshooting, Developer guide).
- Kernel registry: follow the kernel-specific pages for parameter meanings and priors.
- Reference: `?build_nimble_bundle`, `?predict.mixgpd_fit`, `?cqte`.

## Notes

- Each kernel's mixture index is denoted by `j`, and the components argument is `J`.
- `GPD = TRUE/FALSE` controls whether the tail module is active; prediction reuses the sampling weights from the same run so diagnostics stay aligned.
