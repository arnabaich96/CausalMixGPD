# Manual Overview

## DPmixGPD User Manual

Welcome to the DPmixGPD user manual. This section provides comprehensive
documentation for using the package effectively.

### Getting Started

Begin here if you’re new to DPmixGPD:

- [**Introduction**](https://arnabaich96.github.io/DPmixGPD/articles/introduction.html) -
  Quick start guide with basic examples to get you up and running

### User Guide

Detailed documentation organized by topic:

#### Core Concepts

- [**Available
  Distributions**](https://arnabaich96.github.io/DPmixGPD/articles/distributions.html) -
  Overview of supported mixture kernels (Normal, Lognormal, Gamma, etc.)
  and their properties

- [**Model
  Specification**](https://arnabaich96.github.io/DPmixGPD/articles/model-spec.html) -
  How to specify models, set priors, and configure parameters

- [**MCMC
  Workflow**](https://arnabaich96.github.io/DPmixGPD/articles/mcmc-workflow.html) -
  Running MCMC chains, convergence diagnostics, and best practices

#### Model Types

- [**Unconditional
  Models**](https://arnabaich96.github.io/DPmixGPD/articles/unconditional.html) -
  Density estimation without covariates for univariate data

- [**Conditional
  Models**](https://arnabaich96.github.io/DPmixGPD/articles/conditional.html) -
  Covariate-dependent density estimation using parameter linking

- [**Causal
  Inference**](https://arnabaich96.github.io/DPmixGPD/articles/causal.html) -
  Treatment effect estimation with propensity score adjustment

#### Advanced Topics

- [**Backends (CRP vs
  SB)**](https://arnabaich96.github.io/DPmixGPD/articles/backends.html) -
  Comparing Chinese Restaurant Process and Stick-Breaking inference
  approaches

- [**S3 Methods
  Reference**](https://arnabaich96.github.io/DPmixGPD/articles/reference-s3.html) -
  Complete guide to using
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html), and other S3
  methods

### How to Use This Manual

1.  **New users**: Start with the
    [Introduction](https://arnabaich96.github.io/DPmixGPD/articles/introduction.html)
    for a quick overview
2.  **Building models**: Read [Model
    Specification](https://arnabaich96.github.io/DPmixGPD/articles/model-spec.html)
    and [MCMC
    Workflow](https://arnabaich96.github.io/DPmixGPD/articles/mcmc-workflow.html)
3.  **Specific use cases**: Jump to
    [Unconditional](https://arnabaich96.github.io/DPmixGPD/articles/unconditional.html),
    [Conditional](https://arnabaich96.github.io/DPmixGPD/articles/conditional.html),
    or
    [Causal](https://arnabaich96.github.io/DPmixGPD/articles/causal.html)
    based on your needs
4.  **API details**: See [S3 Methods
    Reference](https://arnabaich96.github.io/DPmixGPD/articles/reference-s3.html)
    for comprehensive method documentation
