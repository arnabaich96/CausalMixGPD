# Kernel reference: Normal

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Normal** kernel is parameterized by mean `mean` and standard
deviation `sd`.

## Theory (brief)

For parameters \$\\mu\$ and \$\\sigma\$, the kernel density is \$\$ f(y)
=
\\frac{1}{\\sqrt{2\\pi}\\sigma}\\exp\\left(-\\frac{(y-\\mu)^2}{2\\sigma^2}\\right).
\$\$

## Exported mixture helpers

For the stick-breaking (SB) backend, the package provides vectorized
mixture helpers:

- `dNormMix(x, mean, sd, w, log = FALSE)`
- `pNormMix(q, mean, sd, w, lower.tail = TRUE, log.p = FALSE)`
- `qNormMix(p, mean, sd, w, lower.tail = TRUE, log.p = FALSE)`
- `rNormMix(n, mean, sd, w)`

For a full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/kernels/v02-available-distributions.md)

## Using Normal in a model

``` r
data("faithful", package = "datasets")
y <- faithful$eruptions
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  components = 6
)
```
