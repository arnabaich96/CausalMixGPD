# Model Specification

## Overview

This vignette documents the inputs that define a DPmixGPD model and how
they map to a compiled NIMBLE bundle.

## Core Constructor

Most workflows begin with
[`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.html).

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

## Key Arguments

| Argument |   Value   |                     Description                     |
|:--------:|:---------:|:---------------------------------------------------:|
| backend  |    sb     |     Stick-breaking mixture (finite truncation)      |
| backend  |    crp    |         Chinese Restaurant Process mixture          |
|  kernel  | (various) | Bulk component family (normal, gamma, lognormal, …) |
|   GPD    |   TRUE    |          Splice GPD tail beyond threshold           |
|   GPD    |   FALSE   |                   Bulk-only model                   |

Model Specification Arguments

## Model Types

| Type | Specification | Prediction Behavior |
|----|----|----|
| **Unconditional** | Omit `X` or pass `X = NULL` | Population-level, replicated across observations |
| **Conditional** | Provide `X` | Computed per-row of `X` |

## Input Requirements

- For conditional models, `length(y)` must equal `nrow(X)`
- Covariates must be numeric for regression-style links
- Covariate names must not use NIMBLE reserved keywords (`if`, `for`,
  `while`, etc.)

``` r
# Rename reserved keywords
names(X)[names(X) == "if"] <- "x_if"
```
