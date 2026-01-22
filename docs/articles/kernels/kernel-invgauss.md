# Kernel reference: Inverse Gaussian

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Inverse Gaussian** kernel is parameterized by `mean` and `shape`.

## Exported mixture helpers

- `dInvGaussMix(x, mean, shape, w, log = FALSE)`
- `pInvGaussMix(q, mean, shape, w, lower.tail = TRUE, log.p = FALSE)`
- `qInvGaussMix(p, mean, shape, w, lower.tail = TRUE, log.p = FALSE)`
- `rInvGaussMix(n, mean, shape, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/kernels/v02-available-distributions.html)

## Using Inverse Gaussian in a model

``` r
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel  = "invgauss",
  GPD     = TRUE,
  components = 6
)
```
