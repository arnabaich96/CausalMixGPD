# Cookbook (Example Suite)

## Cookbook Overview

This section is a complete cookbook of DPmixGPD model combinations.
Each vignette is a **recipe**: a specific pairing of backend, tail
handling, and model type with a worked example.

> **Note**: For the most up-to-date documentation, please refer to the
> [Vignettes](https://arnabaich96.github.io/DPmixGPD/articles/manual-index.html)
> section.

### Getting Started Recipes

- [**v00 - Start
  Here**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v00-start-here.html) -
  Original quick start guide
- [**v01 -
  Introduction**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v01-introduction.html) -
  Package overview and motivation
- [**v02 - Available
  Distributions**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v02-available-distributions.html) -
  Detailed distribution catalog with visualizations

### Core Concepts Recipes

- [**v03 - Basic Model Compile &
  Run**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v03-basic-model-compile-run.html) -
  Step-by-step model building
- [**v04 - Backends and
  Workflow**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v04-backends-and-workflow.html) -
  CRP vs SB backend comparison

### Unconditional Model Recipes

Density estimation without covariates:

- [**v05 - DPmix
  CRP**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v05-unconditional-DPmix-CRP.html) -
  Basic CRP mixture model
- [**v06 - DPmix CRP
  Extended**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v06-unconditional-DPmix-CRP.html) -
  Extended CRP examples with diagnostics
- [**v07 - DPmix
  SB**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v07-unconditional-DPmix-SB.html) -
  Stick-Breaking mixture model
- [**v08 - DPmixGPD
  CRP**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v08-unconditional-DPmixGPD-CRP.html) -
  CRP with GPD tail
- [**v09 - DPmixGPD
  SB**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v09-unconditional-DPmixGPD-SB.html) -
  Stick-Breaking with GPD tail

### Conditional Model Recipes

Covariate-dependent density estimation:

- [**v10 - DPmix
  CRP**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v10-conditional-DPmix-CRP.html) -
  Conditional CRP mixture
- [**v11 - DPmix
  SB**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v11-conditional-DPmix-SB.html) -
  Conditional Stick-Breaking mixture
- [**v12 - DPmixGPD
  CRP**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v12-conditional-DPmixGPD-CRP.html) -
  Conditional CRP with GPD tail
- [**v13 - DPmixGPD
  SB**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v13-conditional-DPmixGPD-SB.html) -
  Conditional Stick-Breaking with GPD tail

### Causal Inference Recipes

Treatment effect estimation:

- [**v14 - No Covariates
  CRP**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v14-causal-no-x-CRP.html) -
  Causal inference without confounders
- [**v15 - With Covariates
  SB**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v15-causal-x-no-ps-SB.html) -
  Causal inference with covariates
- [**v16 - Same Backend
  CRP**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v16-causal-same-backend-CRP.html) -
  Treatment/control with same CRP backend
- [**v17 - Same Backend
  SB**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v17-causal-same-backend-SB.html) -
  Treatment/control with same SB backend
- [**v18 - Different Backends
  CRP**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v18-causal-different-backends-CRP.html) -
  Mixed backend causal analysis
- [**v19 - Different Backends
  SB**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v19-causal-different-backends-SB.html) -
  Mixed backend causal analysis

### Troubleshooting Recipes

- [**v20 -
  Troubleshooting**](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v20-troubleshooting.html) -
  Common issues and solutions

### Why a Cookbook?

These recipes were created during earlier development phases and
contain detailed end-to-end examples. While the core concepts remain
valid, some function names, arguments, or workflows may have changed.
Always check the current
[Vignettes](https://arnabaich96.github.io/DPmixGPD/articles/manual-index.html)
and [Function
Reference](https://arnabaich96.github.io/DPmixGPD/reference/index.html)
for the latest API.
