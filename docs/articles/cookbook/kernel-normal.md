# Kernel reference: Normal

> **Cookbook vignette (for the website / historical notes).** These
> files may not match the current exported API one-to-one. Last
> verified: **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Normal** kernel is parameterized by mean `mean` and standard
deviation `sd`.

## Exported mixture helpers

For the stick-breaking (SB) backend, the package provides vectorized
mixture helpers:

- `dNormMix(x, mean, sd, w, log = FALSE)`
- `pNormMix(q, mean, sd, w, lower.tail = TRUE, log.p = FALSE)`
- `qNormMix(p, mean, sd, w, lower.tail = TRUE, log.p = FALSE)`
- `rNormMix(n, mean, sd, w)`

For a full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/cookbook/v02-available-distributions.md)

## Using Normal in a model

``` r

y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  components = 6
)
```
