# Kernel reference: Amoroso

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Amoroso** kernel is parameterized by (`a`, `theta`, `alpha`,
`beta`).

## Exported mixture helpers

- `dAmorosoMix(x, a, theta, alpha, beta, w, log = FALSE)`
- `pAmorosoMix(q, a, theta, alpha, beta, w, lower.tail = TRUE, log.p = FALSE)`
- `qAmorosoMix(p, a, theta, alpha, beta, w, lower.tail = TRUE, log.p = FALSE)`
- `rAmorosoMix(n, a, theta, alpha, beta, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/kernels/v02-available-distributions.html)

## Using Amoroso in a model

``` r
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "amoroso",
  GPD     = TRUE,
  components = 6
)
```
