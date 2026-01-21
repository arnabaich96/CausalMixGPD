# DPmixGPD
[![codecov](https://codecov.io/gh/arnabaich96/DPmixGPD/branch/master/graph/badge.svg?token=80IYA0FANW)](https://codecov.io/gh/arnabaich96/DPmixGPD)

**DPmixGPD** fits Dirichlet process mixture models (CRP or stick-breaking backends) with an optional Generalized Pareto (GPD) tail, enabling unified modeling of bulk distributions and extreme values with clean, user-friendly prediction APIs.

## Installation

```r
# install.packages("remotes")
remotes::install_github("arnabaich96/DPmixGPD")
```

## Documentation

### Manual

Comprehensive user documentation:

- [**Manual Overview**](https://arnabaich96.github.io/DPmixGPD/articles/manual-index.html) - Complete guide to using DPmixGPD
- [**Introduction**](https://arnabaich96.github.io/DPmixGPD/articles/introduction.html) - Quick start guide with basic examples
- [**Available Distributions**](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html) - Overview of supported mixture kernels
- [**Model Specification**](https://arnabaich96.github.io/DPmixGPD/articles/model-spec.html) - How to specify models and priors
- [**MCMC Workflow**](https://arnabaich96.github.io/DPmixGPD/articles/mcmc-workflow.html) - Running and diagnosing MCMC chains
- [**Unconditional Models**](https://arnabaich96.github.io/DPmixGPD/articles/unconditional.html) - Density estimation without covariates
- [**Conditional Models**](https://arnabaich96.github.io/DPmixGPD/articles/conditional.html) - Covariate-dependent density estimation
- [**Causal Inference**](https://arnabaich96.github.io/DPmixGPD/articles/causal.html) - Treatment effect estimation with propensity scores
- [**Backends (CRP vs SB)**](https://arnabaich96.github.io/DPmixGPD/articles/backends.html) - Comparing inference approaches
- [**S3 Methods Reference**](https://arnabaich96.github.io/DPmixGPD/articles/reference-s3.html) - Using print, summary, plot, predict, etc.

### Kernels

- [**Kernels Overview**](https://arnabaich96.github.io/DPmixGPD/articles/kernels-index.html) - Guide to available mixture kernels
- [Amoroso](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-amoroso.html) | [Cauchy](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-cauchy.html) | [Gamma](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-gamma.html) | [Inverse Gaussian](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-invgauss.html) | [Laplace](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-laplace.html) | [Lognormal](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-lognormal.html) | [Normal](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-normal.html)

### Legacy

- [**Legacy Documentation**](https://arnabaich96.github.io/DPmixGPD/articles/legacy-index.html) - Previous version tutorials and detailed examples

### Reference

- [**Function Reference**](https://arnabaich96.github.io/DPmixGPD/reference/index.html) - Complete API documentation
