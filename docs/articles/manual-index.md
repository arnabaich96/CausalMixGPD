# Manual Overview

## Pantry overview (DPmixGPD manual)

Welcome to the pantry: recipes and guides for using DPmixGPD.

### First steps in the kitchen

Start here if you’re new:

- [**Introduction**](https://arnabaich96.github.io/DPmixGPD/articles/introduction.md) -
  Quick start with basic examples

### Recipes by topic

#### Core concepts

- [**Available
  Distributions**](https://arnabaich96.github.io/DPmixGPD/articles/distributions.md) -
  Overview of supported mixture kernels (Normal, Lognormal, Gamma, etc.)
  and their properties

- [**Model
  Specification**](https://arnabaich96.github.io/DPmixGPD/articles/model-spec.md) -
  How to specify models, set priors, and configure parameters

- [**MCMC
  Workflow**](https://arnabaich96.github.io/DPmixGPD/articles/mcmc-workflow.md) -
  Running MCMC chains, convergence diagnostics, and best practices

#### Model types

- [**Unconditional
  Models**](https://arnabaich96.github.io/DPmixGPD/articles/unconditional.md) -
  Density estimation without covariates for univariate data

- [**Conditional
  Models**](https://arnabaich96.github.io/DPmixGPD/articles/conditional.md) -
  Covariate-dependent density estimation using parameter linking

- [**Causal
  Inference**](https://arnabaich96.github.io/DPmixGPD/articles/causal.md) -
  Treatment effect estimation with propensity score adjustment

- [**Umbrella: Custom Models
  (All-in-One)**](https://arnabaich96.github.io/DPmixGPD/articles/allin1/umbrella.md) -
  Consolidated custom model recipes for CRP/SB, GPD, and causal
  workflows

- **Customization: Build NIMBLE Models from Scratch** - Manual SB/CRP
  workflows using exported nimbleFunctions with full NIMBLE pipelines

#### Advanced topics

- [**Backends (CRP vs
  SB)**](https://arnabaich96.github.io/DPmixGPD/articles/backends.md) -
  Comparing Chinese Restaurant Process and Stick-Breaking inference
  approaches

- [**S3 Methods
  Reference**](https://arnabaich96.github.io/DPmixGPD/articles/reference-s3.md) -
  Complete guide to using
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  `if (interactive()) plot()`,
  [`predict()`](https://rdrr.io/r/stats/predict.html), and other S3
  methods

### How to use this pantry

1.  **New to the kitchen**: Start with the
    [Introduction](https://arnabaich96.github.io/DPmixGPD/articles/introduction.md)
2.  **Building recipes**: Read [Model
    Specification](https://arnabaich96.github.io/DPmixGPD/articles/model-spec.md)
    and [MCMC
    Workflow](https://arnabaich96.github.io/DPmixGPD/articles/mcmc-workflow.md)
3.  **What to cook**: Jump to
    [Unconditional](https://arnabaich96.github.io/DPmixGPD/articles/unconditional.md),
    [Conditional](https://arnabaich96.github.io/DPmixGPD/articles/conditional.md),
    or
    [Causal](https://arnabaich96.github.io/DPmixGPD/articles/causal.md)
    as needed
4.  **Utensils**: See [S3 Methods
    Reference](https://arnabaich96.github.io/DPmixGPD/articles/reference-s3.md)
    for method details

### NIMBLE Version Notes

DPmixGPD is developed and tested with **nimble 1.4.0** (see
`renv.lock`). Other recent nimble releases should work, but if you
encounter compilation or sampler issues, please match the tested version
first.
