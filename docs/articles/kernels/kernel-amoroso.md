# Kernel reference: Amoroso

> **Cookbook vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Amoroso** kernel is parameterized by (`a`, `theta`, `alpha`,
`beta`).

## Theory (brief)

The Amoroso distribution is a flexible positive-support family that
generalizes gamma and Weibull-type shapes. Its parameters control
location, scale, and tail behavior, making it useful when standard
parametric kernels are too rigid.

## Exported mixture helpers

- `dAmorosoMix(x, a, theta, alpha, beta, w, log = FALSE)`
- `pAmorosoMix(q, a, theta, alpha, beta, w, lower.tail = TRUE, log.p = FALSE)`
- `qAmorosoMix(p, a, theta, alpha, beta, w, lower.tail = TRUE, log.p = FALSE)`
- `rAmorosoMix(n, a, theta, alpha, beta, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/kernels/v02-available-distributions.md)

## Using Amoroso in a model

``` r
data("faithful", package = "datasets")
y <- faithful$eruptions
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "amoroso",
  GPD     = TRUE,
  components = 6
)
```
