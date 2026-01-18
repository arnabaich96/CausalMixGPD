# Kernel reference: Cauchy

The **Cauchy** kernel is parameterized by `location` and `scale`.

## Exported mixture helpers

- `dCauchyMix(x, location, scale, w, log = FALSE)`
- `pCauchyMix(q, location, scale, w, lower.tail = TRUE, log.p = FALSE)`
- `qCauchyMix(p, location, scale, w, lower.tail = TRUE, log.p = FALSE)`
- `rCauchyMix(n, location, scale, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/v02-available-distributions.md)

## Using Cauchy in a model

``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel  = "cauchy",
  GPD     = TRUE,
  Kmax    = 6
)
```
