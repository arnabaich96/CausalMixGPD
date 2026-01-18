# Kernel reference: Gamma

The **Gamma** kernel is parameterized by `shape` and `rate`.

## Exported mixture helpers

- `dGammaMix(x, shape, rate, w, log = FALSE)`
- `pGammaMix(q, shape, rate, w, lower.tail = TRUE, log.p = FALSE)`
- `qGammaMix(p, shape, rate, w, lower.tail = TRUE, log.p = FALSE)`
- `rGammaMix(n, shape, rate, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/v02-available-distributions.md)

## Using Gamma in a model

``` r
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel  = "gamma",
  GPD     = TRUE,
  components = 6
)
```
