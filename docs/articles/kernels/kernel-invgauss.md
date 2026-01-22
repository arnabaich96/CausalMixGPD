# Kernel reference: Inverse Gaussian

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Inverse Gaussian** kernel is parameterized by `mean` and `shape`.

## Theory (brief)

The inverse Gaussian is a positive-support distribution with density
\$\$ f(y) = \\left(\\frac{\\lambda}{2\\pi y^3}\\right)^{1/2}
\\exp\\left(-\\frac{\\lambda (y-\\mu)^2}{2\\mu^2 y}\\right), \$\$ where
\$\\mu\$ is the mean and \$\\lambda\$ is the shape parameter.

## Exported mixture helpers

- `dInvGaussMix(x, mean, shape, w, log = FALSE)`
- `pInvGaussMix(q, mean, shape, w, lower.tail = TRUE, log.p = FALSE)`
- `qInvGaussMix(p, mean, shape, w, lower.tail = TRUE, log.p = FALSE)`
- `rInvGaussMix(n, mean, shape, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/kernels/v02-available-distributions.md)

## Using Inverse Gaussian in a model

``` r
data("faithful", package = "datasets")
y <- faithful$eruptions
bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel  = "invgauss",
  GPD     = TRUE,
  components = 6
)
```
