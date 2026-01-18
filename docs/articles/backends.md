# Backends

## Two backends, same model

DPmixGPD offers two equivalent representations of the same mixture
model:

- **SB (stick‑breaking)**
- **CRP (Chinese restaurant process)**

## SB vs CRP: conceptual comparison

| Aspect         | SB                          | CRP                           |
|----------------|-----------------------------|-------------------------------|
| Components     | fixed truncation $`K`$      | capped but adaptive occupancy |
| Computation    | predictable, vectorized     | flexible clustering           |
| Interpretation | weights from stick‑breaking | cluster allocation process    |

## When to use each

- Use **SB** when runtime and memory predictability matter.
- Use **CRP** when you want adaptive cluster occupancy within the same
  $`K`$.

## Computational tradeoffs

- SB often runs faster per iteration.
- CRP can mix better for clustering structure.

## Minimal example (non‑executed)

``` r
bundle_sb <- build_nimble_bundle(y = y, kernel = "gamma", backend = "sb", J = 5)
bundle_crp <- build_nimble_bundle(y = y, kernel = "gamma", backend = "crp", J = 5)
```

This vignette is intentionally short. For model building, see
[model-spec](https://arnabaich96.github.io/DPmixGPD/articles/model-spec.Rmd).
