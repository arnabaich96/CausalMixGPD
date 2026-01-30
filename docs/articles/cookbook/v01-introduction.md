# 1. Introduction to DPmixGPD

> **Cookbook vignette (for the website / historical notes).** These
> files may not match the current exported API one-to-one. Last
> verified: **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

### Theory (brief)

DP mixtures represent densities as \$f(y)=\\int
K(y;\\theta)\\,dG(\\theta)\$ with \$G \\sim \\mathrm{DP}(\\alpha,
G_0)\$. Optional GPD splicing replaces the tail beyond a threshold,
providing principled extreme-value behavior.

## DPmixGPD: Bayesian Nonparametric Mixture Models with GPD Tails

### Overview

**DPmixGPD** is an R package for Bayesian nonparametric Dirichlet
Process mixture models with flexible kernels and optional Generalized
Pareto Distribution (GPD) tail augmentation.

#### Key Features

- **Flexible mixture kernels**: normal, gamma, lognormal, laplace,
  invgauss, amoroso, cauchy
- **Two MCMC backends**: Stick-Breaking (SB) and Chinese Restaurant
  Process (CRP)
- **Optional GPD tail**: Enhance tail probability estimation
- **Conditional modeling**: Add covariates via parameter linking
- **Causal workflows**: Propensity score adjustment with multiple PS
  strategies
- **Comprehensive prediction**: density, survival, quantile, sample,
  mean

------------------------------------------------------------------------

### Model Structure

#### Single-Outcome Model

**Bulk**: Dirichlet Process mixture
``` math
Y_i \sim \sum_{k=1}^{\infty} w_k f_k(y_i; \theta_k)
```

**Tail (optional)**: GPD beyond threshold
``` math
P(Y > u | Y > u) \sim \text{GPD}(\text{scale}, \text{shape})
```

#### Causal Model

Two outcome arms with optional propensity score (PS) adjustment: -
**Control** ($`T=0`$):
$`Y_i^{(0)} \sim \text{DPM}_{\text{con}}(X_i, PS_i)`$ - **Treated**
($`T=1`$): $`Y_i^{(1)} \sim \text{DPM}_{\text{trt}}(X_i, PS_i)`$

------------------------------------------------------------------------

### MCMC Backends

#### Stick-Breaking (SB)

- Fixed number of mixture components `components`
- Exact truncation at the chosen `components`
- Faster per-iteration computation
- Use when computational cost is primary concern

#### Chinese Restaurant Process (CRP)

- Finite `components` cap for sampling
- Adaptively discovers number of occupied clusters within that cap
- More flexible cluster allocation
- Default for exploratory analysis

------------------------------------------------------------------------

### Propensity Score (PS) Models

**Three PS strategies** (causal workflows only):

1.  **`PS="logit"`** (default): Bayesian logistic regression
2.  **`PS="probit"`**: Bayesian probit regression
3.  **`PS="naive"`**: Gaussian naive Bayes classifier
4.  **`PS=FALSE`**: No PS; outcomes depend on $`X`$ only

When PS is estimated, scores are injected into outcome model covariates
for confounding adjustment.

------------------------------------------------------------------------

### Workflow Overview

#### Direct Model Building

``` r

bundle <- build_nimble_bundle(
  y = y,
  kernel = "gamma",
  backend = "crp",
  GPD = TRUE,
  components = 5,
  mcmc = mcmc
)
fit <- run_mcmc_bundle_manual(bundle)
```

------------------------------------------------------------------------

### Quick Start

``` r

# Load packaged dataset
data("nc_pos200_k3")
y <- nc_pos200_k3$y

# Build unconditional model: bulk-only, SB backend
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "gamma",
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)

# Print bundle
print(bundle)
```

    DPmixGPD bundle
          Field                  Value
        Backend Stick-Breaking Process
         Kernel     Gamma Distribution
     Components                      5
              N                    200
              X                     NO
            GPD                  FALSE
        Epsilon                  0.025

      contains  : code, constants, data, dimensions, inits, monitors

------------------------------------------------------------------------

### Outline of This Vignette Series

1.  **Introduction** (this vignette)
2.  **Available Distributions**: dqrp functions, 10k samples,
    visualizations
3.  **Basic Model Workflow**: compile → bundle → MCMC
4.  **Unconditional DPmix (CRP)**: bulk-only, no covariates
5.  **Unconditional DPmix (SB)**: bulk-only, no covariates
6.  **Unconditional DPmixGPD (CRP)**: bulk+tail, no covariates
7.  **Unconditional DPmixGPD (SB)**: bulk+tail, no covariates
8.  **Conditional DPmix (CRP)**: bulk-only, with covariates
9.  **Conditional DPmix (SB)**: bulk-only, with covariates
10. **Conditional DPmixGPD (CRP)**: bulk+tail, with covariates
11. **Conditional DPmixGPD (SB)**: bulk+tail, with covariates
12. **Causal (Same Backend CRP)**: PS + treatment effects, CRP
13. **Causal (Same Backend SB)**: PS + treatment effects, SB
14. **Causal (Different Backends CRP)**: Per-arm backends, CRP
15. **Causal (Different Backends SB)**: Per-arm backends, SB

------------------------------------------------------------------------

### Dependencies

``` r

packageVersion("DPmixGPD")
```

    [1] '0.0.9'

``` r

packageVersion("nimble")
```

    [1] '1.4.0'

``` r

packageVersion("ggplot2")
```

    [1] '4.0.1'

Each vignette demonstrates complete workflows with reproducible
examples.
