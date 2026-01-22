# Kernel reference: Gamma

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Gamma** kernel is parameterized by `shape` and `rate`.

## Theory (brief)

For shape $`k`$ and rate \$\\beta\$, the gamma density on $`y>0`$ is
\$\$ f(y) = \\frac{\\beta^k}{\\Gamma(k)} y^{k-1} e^{-\\beta y}. \$\$

## Exported mixture helpers

- `dGammaMix(x, shape, rate, w, log = FALSE)`
- `pGammaMix(q, shape, rate, w, lower.tail = TRUE, log.p = FALSE)`
- `qGammaMix(p, shape, rate, w, lower.tail = TRUE, log.p = FALSE)`
- `rGammaMix(n, shape, rate, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/kernels/v02-available-distributions.md)

## Using Gamma in a model

``` r
data("faithful", package = "datasets")
y <- faithful$eruptions
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "gamma",
  GPD     = TRUE,
  components = 6
)
```
