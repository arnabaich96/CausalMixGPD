# Wrap nimbleCode for bundle storage

Wrap nimbleCode for bundle storage

## Usage

``` r
.wrap_nimble_code(code)
```

## Details

This helper is the inverse of
[`.extract_nimble_code()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dot-extract_nimble_code.md).
It stores a raw `nimbleCode` object inside a lightweight list so bundle
objects can carry code alongside other metadata without ambiguity about
the field layout.
