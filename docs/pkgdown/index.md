# DPmixGPD

[![codecov](https://codecov.io/gh/arnabaich96/DPmixGPD/branch/master/graph/badge.svg?token=80IYA0FANW)](https://codecov.io/gh/arnabaich96/DPmixGPD)

**DPmixGPD** — Dirichlet process mixture models (CRP or stick-breaking)
with an optional GPD tail: one kitchen for bulk distributions and
extreme values, with clean prediction APIs.

## Get it in the kitchen

``` r



# install.packages("remotes")
remotes::install_github("arnabaich96/DPmixGPD")
```

## Kitchen map

- [**Package
  roadmap**](https://arnabaich96.github.io/DPmixGPD/pkgdown/ROADMAP.md)
  — Levels of the analysis process and key functions

### Pantry (Manual)

Recipes and guides for using DPmixGPD:

- [**Pantry
  Overview**](https://arnabaich96.github.io/DPmixGPD/articles/manual-index.html) -
  Complete guide to using DPmixGPD
- [**Introduction**](https://arnabaich96.github.io/DPmixGPD/articles/introduction.html) -
  Quick start with basic examples
- [**Ingredient list
  (Distributions)**](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html) -
  Supported mixture kernels
- [**Recipe spec
  (Model)**](https://arnabaich96.github.io/DPmixGPD/articles/model-spec.html) -
  Specifying models and priors
- [**Prep and cook
  (MCMC)**](https://arnabaich96.github.io/DPmixGPD/articles/mcmc-workflow.html) -
  Running and diagnosing MCMC
- [**Unconditional**](https://arnabaich96.github.io/DPmixGPD/articles/unconditional.html) -
  Density estimation without covariates
- [**Conditional**](https://arnabaich96.github.io/DPmixGPD/articles/conditional.html) -
  Covariate-dependent density estimation
- [**Causal**](https://arnabaich96.github.io/DPmixGPD/articles/causal.html) -
  Treatment effects with propensity scores
- [**Oven choice (CRP vs
  SB)**](https://arnabaich96.github.io/DPmixGPD/articles/backends.html) -
  Comparing inference approaches
- [**Utensil guide (S3
  methods)**](https://arnabaich96.github.io/DPmixGPD/articles/reference-s3.html) -
  print, summary, plot, predict, etc.

### Ingredients (Kernels)

- [**Ingredient
  overview**](https://arnabaich96.github.io/DPmixGPD/articles/kernels-index.html) -
  Available mixture kernels
- [Amoroso](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html#amoroso)
  \|
  [Cauchy](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html#cauchy)
  \|
  [Gamma](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html#gamma)
  \| [Inverse
  Gaussian](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html#inverse-gaussian)
  \|
  [Laplace](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html#laplace)
  \|
  [Lognormal](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html#lognormal)
  \|
  [Normal](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html#normal)

### Cookbook

- [**Recipe
  Book**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook-index.html) -
  Step-by-step tutorials and case studies

### Reference

- [**Function
  reference**](https://arnabaich96.github.io/DPmixGPD/reference/index.html) -
  Full API documentation

### Coverage

- ![Coverage
  sunburst](https://codecov.io/gh/arnabaich96/DPmixGPD/graphs/sunburst.svg?token=80IYA0FANW)
  Coverage sunburst
