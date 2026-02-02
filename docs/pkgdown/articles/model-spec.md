# Model Specification

## Overview

This vignette describes the probabilistic model behind DPmixGPD and how
user inputs map to a compiled NIMBLE bundle.

## Theory (core model)

### Bulk mixture

For outcomes $`y_i`$ and kernel $`K`$, the bulk distribution is modeled
as a DP mixture: \$\$ f(y_i) = \\int K(y_i; \\theta)\\, dG(\\theta),
\\quad G \\sim \\mathrm{DP}(\\alpha, G_0). \$\$ The DP prior on $`G`$
induces clustering of component parameters and allows the mixture to
adapt to unknown distributional shapes.

### Tail augmentation (optional)

When \$\\mathrm{GPD} = \\mathrm{TRUE}\$, a Generalized Pareto tail
replaces the bulk kernel beyond a threshold $`u`$: \$\$ f(y) =
\\begin{cases} f\_{\\text{bulk}}(y), & y \\le u \\\\ f\_{\\text{GPD}}(y;
u, \\sigma, \\xi), & y \> u \\end{cases} \$\$ with continuity enforced
at the threshold.

### Conditional extension

Conditional models allow kernel parameters to depend on covariates
$`x_i`$: \$\$ f(y_i \\mid x_i) = \\int K(y_i; \\theta(x_i))\\,
dG(\\theta). \$\$ The linking structure is kernel-specific and
implemented in the model builder.

## Core constructor

Most workflows begin with
[`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/build_nimble_bundle.md).
The bundle includes compiled NIMBLE code and MCMC settings.

``` r

library(DPmixGPD)

data("faithful", package = "datasets")
y <- faithful$eruptions

bundle_uncond <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  components = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 1, seed = 1)
)
```

## Conditional example

``` r

data("mtcars", package = "datasets")
df <- mtcars
X <- df[, c("wt", "hp")]
X <- as.data.frame(X)
y <- df$mpg

bundle_cond <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  components = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 1, seed = 1)
)
```

## Key arguments

| Argument | Value | Description |
|:--:|:--:|:--:|
| backend | sb | Stick-breaking mixture (finite truncation) |
| backend | crp | Chinese Restaurant Process mixture |
| kernel | (various) | Bulk component family (normal, gamma, lognormal, …) |
| GPD | TRUE | Splice GPD tail beyond threshold |
| GPD | FALSE | Bulk-only model |
| components | K | Truncation level for SB (maximum components) |
| alpha_random | TRUE/FALSE | Learn concentration parameter (DP strength) |

Model Specification Arguments {.table .table .table-striped .table-hover
style="width: auto !important; margin-left: auto; margin-right: auto;"}

## Model types

| Type | Specification | Prediction Behavior |
|----|----|----|
| **Unconditional** | Omit `X` or pass `X = NULL` | Population-level, replicated across observations |
| **Conditional** | Provide `X` | Computed per-row of `X` |

## Input requirements

- For conditional models, `length(y)` must equal `nrow(X)`
- Covariates must be numeric for regression-style links
- Covariate names must not use NIMBLE reserved keywords (`if`, `for`,
  `while`, etc.)

``` r

# Rename reserved keywords
names(X)[names(X) == "if"] <- "x_if"
```
