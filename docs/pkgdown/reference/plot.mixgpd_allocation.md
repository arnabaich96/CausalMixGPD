# Plot cluster allocation

Creates diagnostic plots for cluster allocations, including cluster
sizes, response distributions by cluster, and allocation certainty.

## Usage

``` r
# S3 method for class 'mixgpd_allocation'
plot(x, overlay = TRUE, ...)
```

## Arguments

- x:

  An object of class `mixgpd_allocation`.

- overlay:

  Logical; if `TRUE` (default) and new data is present, overlay training
  and new data in the same plots. If `FALSE`, show only new data plots
  when new data is present.

- ...:

  Additional arguments passed to plotting functions.

## Value

Invisibly returns a list of plot data.
