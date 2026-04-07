# Print method for paired causal-fit diagnostic plots

Print method for paired causal-fit diagnostic plots

## Usage

``` r
# S3 method for class 'causalmixgpd_causal_fit_plots'
print(x, ...)
```

## Arguments

- x:

  Object of class `causalmixgpd_causal_fit_plots`.

- ...:

  Additional arguments passed to the stored plot-print methods.

## Value

Invisibly returns the input object.

## Details

When
[`plot.causalmixgpd_causal_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/plot.causalmixgpd_causal_fit.md)
is called with `arm = "both"`, the result is a named pair of treated and
control diagnostic-plot objects. This print method renders those two
stored plot collections one after the other so arm-specific diagnostics
remain clearly separated.

It is a formatting helper and does not recompute any diagnostics.
