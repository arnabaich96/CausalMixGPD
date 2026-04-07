# Summarize truncation results from draws

Summarize truncation results from draws

## Usage

``` r
.truncation_info(object, epsilon = NULL)
```

## Arguments

- object:

  A mixgpd_fit.

- epsilon:

  Numeric; optional override.

## Value

List with k summary.

## Details

After
[`.extract_draws()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dot-extract_draws.md)
applies draw-level component truncation, the chosen number of retained
components is stored as an attribute. This helper condenses that
bookkeeping into min, median, and max summaries for the effective
retained component count under both the cumulative-mass and per-weight
criteria.
