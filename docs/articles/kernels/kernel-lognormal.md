# Kernel reference: Lognormal

> **Cookbook vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Lognormal** kernel is parameterized by `meanlog` and `sdlog`
(i.e., `log(y)` is Normal).

## Theory (brief)

If \$Z \\sim \\mathcal{N}(\\mu, \\sigma^2)\$ then \$Y = \\exp(Z)\$ is
lognormal. The kernel has density on $`y>0`$: \$\$ f(y) =
\\frac{1}{y\\sigma\\sqrt{2\\pi}}\\exp\\left(-\\frac{(\\log
y-\\mu)^2}{2\\sigma^2}\\right). \$\$

## Exported mixture helpers

- `dLognormalMix(x, meanlog, sdlog, w, log = FALSE)`
- `pLognormalMix(q, meanlog, sdlog, w, lower.tail = TRUE, log.p = FALSE)`
- `qLognormalMix(p, meanlog, sdlog, w, lower.tail = TRUE, log.p = FALSE)`
- `rLognormalMix(n, meanlog, sdlog, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/kernels/v02-available-distributions.md)

## Using Lognormal in a model

``` r
data("faithful", package = "datasets")
y <- faithful$eruptions
bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel  = "lognormal",
  GPD     = TRUE,
  components = 6
)
```
