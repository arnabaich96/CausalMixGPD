# DPmixGPD: AI Coding Agent Instructions

## Overview

R package (R \>= 4.5.0) for Bayesian nonparametric Dirichlet Process
mixture models with flexible kernels and optional GPD tail augmentation.
Uses NIMBLE for MCMC compilation with two backends: stick-breaking (SB)
and Chinese Restaurant Process (CRP).

## Architecture & Key Patterns

### Three-Phase Model Building (Critical Flow)

``` r
# Phase 1: Specification (validates inputs, NO NIMBLE code generation)
spec <- compile_model_spec(y, kernel = "gamma", backend = "crp", GPD = TRUE)

# Phase 2: Bundle (generates NIMBLE code, compiles sampler)
bundle <- build_nimble_bundle(spec)  # Can also call directly with y, X, kernel, etc.

# Phase 3: MCMC (runs posterior sampling → mixgpd_fit object)
fit <- run_mcmc_bundle_manual(bundle, niter = 1000, nburnin = 100)
```

### Kernel Registry System (Never Hardcode!)

- All kernel properties stored in registry initialized at
  [`.onLoad()`](https://arnabaich96.github.io/DPmixGPD/reference/dot-onLoad.md)
  (see
  [R/00-kernel-registry.R](https://arnabaich96.github.io/DPmixGPD/R/00-kernel-registry.R))
- Available kernels: `gamma`, `lognormal`, `normal`, `laplace`,
  `invgauss`, `amoroso`, `cauchy`
- Query registry before any kernel operation:
  `kreg <- get_kernel_registry(); kernel_def <- kreg[[kernel_name]]`
- Each kernel defines: `bulk_params`, `bulk_support`, `allow_gpd`,
  backend-specific density functions

### Distribution Function Naming Convention

Pattern: `{d|p|q|r} + Kernel + [Mix] + [Gpd]` - `dGamma()` — base kernel
density -
[`dGammaMix()`](https://arnabaich96.github.io/DPmixGPD/reference/gamma_mix.md)
— mixture density (SB backend) -
[`dGammaMixGpd()`](https://arnabaich96.github.io/DPmixGPD/reference/gamma_mixgpd.md)
— mixture with GPD tail - All 4 variants (d/p/q/r) auto-generated for
each kernel

### S3 Classes & Methods

- `mixgpd_fit`: Fitted model with posterior samples (from
  [`run_mcmc_bundle_manual()`](https://arnabaich96.github.io/DPmixGPD/reference/run_mcmc_bundle_manual.md))
- `dpmixgpd_bundle`: Full MCMC bundle before execution (from
  [`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md))
- Standard methods: [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html),
  [`residuals()`](https://rdrr.io/r/stats/residuals.html),
  [`logLik()`](https://rdrr.io/r/stats/logLik.html),
  [`vcov()`](https://rdrr.io/r/stats/vcov.html)
- Plotting via `ggplot2` +
  [`ggmcmc::ggs()`](https://rdrr.io/pkg/ggmcmc/man/ggs.html) for
  trace/density plots

## Developer Workflows

### Essential Commands

``` r
devtools::load_all()        # Load package (no install needed)
devtools::document()        # Regenerate docs + NAMESPACE after @export/@method changes
devtools::check()           # R CMD check + linting
devtools::test()            # Run testthat suite
renv::restore()             # Restore dependencies (renv.lock pinned to R 4.5.2)
```

### Documentation & Vignette Standards

- **Table Formatting**: Always use `kableExtra` wrapper when printing
  tables in vignettes or documentation
  - Example: `kbl(table_data) %>% kable_styling()`
  - Ensures consistent, professional table rendering across HTML and PDF
    outputs
  - Required for all summary tables, parameter tables, and result
    comparisons

### Testing Patterns (See [tests/testthat/test-MCMC.R](https://arnabaich96.github.io/DPmixGPD/tests/testthat/test-MCMC.R))

- Grid test: all kernel × backend × GPD combinations (217 lines)
- Always use `skip_if_not_installed("nimble")` and
  `skip_if_not_installed("coda")`
- Capture noisy output: `utils::capture.output(..., file = nullfile())`
- Snapshot tests in `tests/testthat/_snaps/` for regression checks
- **IMPORTANT**: Never create test files in root directory. Use
  `test_output/` folder for all test scripts and temporary files

### File Organization Logic

- [R/00-kernel-registry.R](https://arnabaich96.github.io/DPmixGPD/R/00-kernel-registry.R):
  Registry initialization
- [R/01-compile-spec.R](https://arnabaich96.github.io/DPmixGPD/R/01-compile-spec.R):
  Model specification validation
- [R/02-utilities-internal.R](https://arnabaich96.github.io/DPmixGPD/R/02-utilities-internal.R):
  MCMC extraction utilities (1089 lines — START HERE for posterior
  processing)
- [R/03-build-and-run.R](https://arnabaich96.github.io/DPmixGPD/R/03-build-and-run.R):
  Bundle building + MCMC execution (main entry point)
- [R/04-S3-Methods.R](https://arnabaich96.github.io/DPmixGPD/R/04-S3-Methods.R):
  All S3 print/summary/plot methods
- [R/1-7-\*.R](https://arnabaich96.github.io/DPmixGPD/R/):
  Kernel-specific distribution functions (grouped by kernel)
- [inst/crp.R](https://arnabaich96.github.io/DPmixGPD/inst/crp.R),
  [inst/sb.R](https://arnabaich96.github.io/DPmixGPD/inst/sb.R): Minimal
  reproducible examples

## Critical Gotchas

1.  **NAMESPACE is auto-generated** — Never edit manually. Use `@export`
    / `@keywords internal @noRd` in roxygen comments
2.  **GPD compatibility** — Not all kernels support GPD tails (check
    `kernel_def$allow_gpd` from registry)
3.  **Global variables** — NIMBLE functions (`ddexp`, `pdexp`, etc.)
    declared in
    [R/globals.R](https://arnabaich96.github.io/DPmixGPD/R/globals.R)
    for R CMD check
4.  **Backend differences**:
    - SB: Uses `*Mix` density functions, fixed `J` components
    - CRP: Uses base density functions (`dnorm`, `dgamma`, etc.), `Kmax`
      truncation
5.  **Spec vs Bundle** — `compile_model_spec()` does NOT generate NIMBLE
    code; use
    [`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md)
    for that

## NIMBLE Integration

- Models compiled via
  [`nimbleModel()`](https://rdrr.io/pkg/nimble/man/nimbleModel.html) +
  [`compileNimble()`](https://rdrr.io/pkg/nimble/man/compileNimble.html)
- Posterior samples converted to
  [`coda::mcmc.list`](https://rdrr.io/pkg/coda/man/mcmc.list.html) for
  diagnostics
- Test failures often due to: invalid node indices, cyclic dependencies,
  or monitor name mismatches

## Quick Example (Unconditional Model)

``` r
set.seed(1)
y <- abs(rnorm(80)) + 0.2
bundle <- build_nimble_bundle(y, backend = "crp", kernel = "gamma", GPD = TRUE, Kmax = 10,
                               mcmc = list(niter = 500, nburnin = 100, nchains = 2))
fit <- run_mcmc_bundle_manual(bundle)
summary(fit)  # Posterior summaries with ESS, Rhat
plot(fit, params = "beta_|threshold")  # Trace/density plots
```
