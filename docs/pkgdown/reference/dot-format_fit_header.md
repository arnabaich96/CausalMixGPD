# Format a short header for printing

Format a short header for printing

## Usage

``` r
.format_fit_header(x)
```

## Arguments

- x:

  A mixgpd_fit.

## Value

Character vector lines.

## Details

This helper builds the short header used by
[`print.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.mixgpd_fit.md).
It extracts model identity, training size, truncation size, epsilon, and
stored MCMC settings into a compact character vector so higher-level
print methods do not duplicate formatting logic.
