# Build a model bundle (short wrapper)

Build a model bundle (short wrapper)

## Usage

``` r
bundle(
  x = NULL,
  data = NULL,
  X = NULL,
  treat = NULL,
  formula = NULL,
  ...,
  GPD = FALSE
)
```

## Arguments

- x:

  Either a response vector or an existing bundle.

- data:

  Optional data.frame used with `formula`.

- X:

  Optional design matrix/data.frame.

- treat:

  Optional binary treatment indicator.

- formula:

  Optional formula.

- ...:

  Additional arguments passed to
  [`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/build_nimble_bundle.md)
  or
  [`build_causal_bundle()`](https://arnabaich96.github.io/DPmixGPD/pkgdown/reference/build_causal_bundle.md).

- GPD:

  Logical; include GPD tail in build mode.

## Value

A `"dpmixgpd_bundle"` or `"dpmixgpd_causal_bundle"`.
