# DPmixGPD
[![codecov](https://codecov.io/gh/arnabaich96/DPmixGPD/branch/master/graph/badge.svg?token=80IYA0FANW)](https://codecov.io/gh/arnabaich96/DPmixGPD)

**DPmixGPD** - Dirichlet process mixture models (CRP or stick-breaking) with an optional GPD tail: one kitchen for bulk distributions and extreme values, with clean prediction APIs.

## Get it in the kitchen

```r
# install.packages("remotes")
remotes::install_github("arnabaich96/DPmixGPD")
```

## Kitchen map

- [**Package roadmap**](ROADMAP.md) - Levels of the analysis process and key functions

### Vignettes

Recipes and guides for using DPmixGPD:

- [**Model specification and posterior computation**](https://arnabaich96.github.io/DPmixGPD/pkgdown/articles/basic.html) - Core model and sampling details
- [**Model fundamentals (bulk DPM + GPD tail)**](https://arnabaich96.github.io/DPmixGPD/pkgdown/articles/model-spec.html) - Formal model definitions and splicing construction
- [**Unconditional**](https://arnabaich96.github.io/DPmixGPD/pkgdown/articles/unconditional.html) - Density estimation without covariates
- [**Conditional**](https://arnabaich96.github.io/DPmixGPD/pkgdown/articles/conditional.html) - Covariate-dependent density estimation
- [**Causal**](https://arnabaich96.github.io/DPmixGPD/pkgdown/articles/causal.html) - Treatment effects with propensity scores

### Ingredients (Kernels)

- [**Ingredient overview**](https://arnabaich96.github.io/DPmixGPD/kernels.html) - Available mixture kernels
- [Kernel docs](https://arnabaich96.github.io/DPmixGPD/kernels/kernels-index.html)

### Examples

- [**Examples**](https://arnabaich96.github.io/DPmixGPD/Examples/index.html) - Step-by-step tutorials and case studies

### Reference

- [**Function reference**](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/index.html) - Full API documentation

### Coverage
- ![Coverage sunburst](https://codecov.io/gh/arnabaich96/DPmixGPD/graphs/sunburst.svg?token=80IYA0FANW)

