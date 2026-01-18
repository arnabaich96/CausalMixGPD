# Kernel reference: Lognormal

The **Lognormal** kernel is parameterized by `meanlog` and `sdlog`
(i.e., `log(y)` is Normal).

## Exported mixture helpers

- `dLognormalMix(x, meanlog, sdlog, w, log = FALSE)`
- `pLognormalMix(q, meanlog, sdlog, w, lower.tail = TRUE, log.p = FALSE)`
- `qLognormalMix(p, meanlog, sdlog, w, lower.tail = TRUE, log.p = FALSE)`
- `rLognormalMix(n, meanlog, sdlog, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/v02-available-distributions.md)

## Using Lognormal in a model

``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel  = "lognormal",
  GPD     = TRUE,
  Kmax    = 6
)
```
