# Kernel reference: Amoroso

The **Amoroso** kernel is parameterized by (`a`, `theta`, `alpha`,
`beta`).

## Exported mixture helpers

- `dAmorosoMix(x, a, theta, alpha, beta, w, log = FALSE)`
- `pAmorosoMix(q, a, theta, alpha, beta, w, lower.tail = TRUE, log.p = FALSE)`
- `qAmorosoMix(p, a, theta, alpha, beta, w, lower.tail = TRUE, log.p = FALSE)`
- `rAmorosoMix(n, a, theta, alpha, beta, w)`

For the full catalog (including CRP utilities and GPD splicing), see:

- [Available
  Distributions](https://arnabaich96.github.io/DPmixGPD/articles/v02-available-distributions.md)

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
