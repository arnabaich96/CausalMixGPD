# Kernel support matrix

Returns a data frame summarizing each kernel's supported features.

## Usage

``` r
kernel_support_table(round = TRUE)
```

## Arguments

- round:

  Logical; `TRUE` to replace logical values with symbols.

## Value

data.frame with columns `kernel`, `gpd`, `covariates`, `sb`, `crp`.

## Details

The returned table is a compact view of the registry contracts. Each row
corresponds to one kernel, while the logical columns indicate whether
that kernel can be paired with a GPD tail, whether covariate-linked
parameter models are defined, and whether the stick-breaking (`sb`) and
Chinese-restaurant (`crp`) backends are implemented.

This helper is intended for inspection and reporting rather than model
fitting. It is a quick way to verify that a requested combination of
kernel, tail, and backend is supported before calling higher-level
workflow constructors.
