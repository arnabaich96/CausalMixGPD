---
title: "Model specification"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Model specification}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Goal

This vignette documents the inputs that define a DPmixGPD model and how they map to a compiled NIMBLE bundle.

**Runtime:** fast (no long MCMC).

## The core constructor

Most workflows start with `build_nimble_bundle()`.


``` r
library(DPmixGPD)

y <- abs(rnorm(40)) + 0.2
X <- data.frame(x = rnorm(40))

bundle <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  components = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 1, seed = 1)
)

bundle
#> DPmixGPD bundle
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Normal Distribution
#>  Components                      6
#>           N                     40
#>           X              YES (P=1)
#>         GPD                   TRUE
#>     Epsilon                  0.025
#> 
#>   contains  : code, constants, data, dimensions, inits, monitors
```

## Key arguments

### `backend`

- `"sb"`: stick-breaking mixture (finite truncation).
- `"crp"`: Chinese Restaurant Process mixture.

### `kernel`

Controls the bulk component family (e.g., `"normal"`, `"gamma"`, `"lognormal"`, ...).

### `GPD`

- `TRUE`: splice a GPD tail beyond a threshold.
- `FALSE`: bulk-only model.

## Conditional vs unconditional

- **Unconditional**: omit `X` (or pass `X = NULL`). Predictions are population-level and replicated across observations.
- **Conditional**: provide `X`. Predictions are computed per-row of `X`.

## Common input hazards

### Reserved variable names (NIMBLE keywords)

If a covariate column is named `if`, `for`, `while`, etc., NIMBLE compilation fails.


``` r
X <- data.frame(if = rnorm(40))
# Fix:
names(X)[names(X) == "if"] <- "x_if"
```

### Shape checks

- `length(y)` must equal `nrow(X)` for conditional models.
- Covariates should be numeric for regression-style links.

## Validity and error messages

Good packages fail loudly and early. If your build step does not yet validate inputs, consider adding checks that:

- confirm supported `backend` and `kernel` values,
- prevent reserved names,
- validate the `mcmc` list.

## Next

- See **MCMC workflow** for running chains and extracting samples.
- See **Backends** for SB vs CRP differences.

