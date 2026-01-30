# Kernels Overview

## Ingredients (mixture kernels)

DPmixGPD supports multiple mixture kernel families for flexible density
estimation. Each kernel has different properties suited to various data
types and modeling scenarios.

### What’s in the pantry

#### Location-scale families

These kernels have location and scale parameters, making them suitable
for real-valued data:

- [**Normal**](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-normal.md) -
  Gaussian kernel with `mean` and `sd` parameters. Best for symmetric,
  bell-shaped distributions.

- [**Cauchy**](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-cauchy.md) -
  Heavy-tailed kernel with `location` and `scale` parameters. Robust to
  outliers but does not support GPD tail.

- [**Laplace**](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-laplace.md) -
  Double exponential kernel with `location` and `scale` parameters. Has
  heavier tails than Normal.

#### Positive-support families

These kernels are defined for positive values only, ideal for positive
data like durations, sizes, or amounts:

- [**Lognormal**](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-lognormal.md) -
  Log-transformed Normal with `meanlog` and `sdlog` parameters. Good for
  right-skewed positive data.

- [**Gamma**](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-gamma.md) -
  Flexible positive kernel with `shape` and `scale` parameters. Can
  model various skewness levels.

- [**Inverse
  Gaussian**](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-invgauss.md) -
  Alternative positive kernel with `mean` and `shape` parameters. Often
  used for waiting times.

#### Generalized families

- [**Amoroso**](https://arnabaich96.github.io/DPmixGPD/articles/kernels/kernel-amoroso.md) -
  Four-parameter generalization with `loc`, `scale`, `shape1`, and
  `shape2`. Includes Gamma, Weibull, and other distributions as special
  cases.

### Ingredient picker

| Data Type                 | Recommended Kernels |
|---------------------------|---------------------|
| Real-valued, symmetric    | Normal, Cauchy      |
| Real-valued, heavy-tailed | Cauchy, Laplace     |
| Positive, right-skewed    | Lognormal, Gamma    |
| Positive, waiting times   | Inverse Gaussian    |
| Positive, flexible shape  | Amoroso, Gamma      |

### GPD Tail Support

Most kernels support GPD tail modeling (`GPD = TRUE`):

| Kernel           | GPD Support |
|------------------|-------------|
| Normal           | Yes         |
| Lognormal        | Yes         |
| Gamma            | Yes         |
| Inverse Gaussian | Yes         |
| Laplace          | Yes         |
| Amoroso          | Yes         |
| Cauchy           | No          |

### Using ingredients in your recipe

Specify the kernel when building your model:

``` r

bundle <- build_nimble_bundle(
  y = your_data,
  backend = "sb",
  kernel = "lognormal",  # Choose your kernel

  GPD = TRUE
)
```

See the individual kernel pages for detailed examples and parameter
descriptions.
