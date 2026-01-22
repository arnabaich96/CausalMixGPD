# Kernel reference: Lognormal

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Lognormal** kernel is parameterized by `meanlog` and `sdlog`
(i.e., `log(y)` is Normal).

## Exported mixture helpers

- `dLognormalMix(x, meanlog, sdlog, w, log = FALSE)`
- `pLognormalMix(q, meanlog, sdlog, w, lower.tail = TRUE, log.p = FALSE)`
- `qLognormalMix(p, meanlog, sdlog, w, lower.tail = TRUE, log.p = FALSE)`
- `rLognormalMix(n, meanlog, sdlog, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/kernels/v02-available-distributions.html)

## Using Lognormal in a model

``` r
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel  = "lognormal",
  GPD     = TRUE,
  components = 6
)
```
