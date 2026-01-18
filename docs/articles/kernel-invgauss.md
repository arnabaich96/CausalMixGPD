# Kernel reference: Inverse Gaussian

The **Inverse Gaussian** kernel is parameterized by `mean` and `shape`.

## Exported mixture helpers

- `dInvGaussMix(x, mean, shape, w, log = FALSE)`
- `pInvGaussMix(q, mean, shape, w, lower.tail = TRUE, log.p = FALSE)`
- `qInvGaussMix(p, mean, shape, w, lower.tail = TRUE, log.p = FALSE)`
- `rInvGaussMix(n, mean, shape, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/v02-available-distributions.md)

## Using Inverse Gaussian in a model

``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel  = "invgauss",
  GPD     = TRUE,
  Kmax    = 6
)
```
