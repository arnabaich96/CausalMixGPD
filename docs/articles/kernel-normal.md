# Kernel reference: Normal

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
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/v02-available-distributions.md)

## Using Normal in a model

``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "normal",
  GPD     = TRUE,
  Kmax    = 6
)
```
