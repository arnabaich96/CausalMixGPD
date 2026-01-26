# Kernel reference: Cauchy

> **Cookbook vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

The **Cauchy** kernel is parameterized by `location` and `scale`.

## Theory (brief)

The Cauchy density is heavy-tailed: \$\$ f(y) =
\\frac{1}{\\pi\\gamma\\left\[1 +
\\left(\\frac{y-x_0}{\\gamma}\\right)^2\\right\]}, \$\$ with location
$`x_0`$ and scale \$\\gamma\$.

## Exported mixture helpers

- `dCauchyMix(x, location, scale, w, log = FALSE)`
- `pCauchyMix(q, location, scale, w, lower.tail = TRUE, log.p = FALSE)`
- `qCauchyMix(p, location, scale, w, lower.tail = TRUE, log.p = FALSE)`
- `rCauchyMix(n, location, scale, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/kernels/v02-available-distributions.md)

## Using Cauchy in a model

``` r
data("faithful", package = "datasets")
y <- faithful$eruptions
bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel  = "cauchy",
  GPD     = FALSE,
  components = 6
)
```
