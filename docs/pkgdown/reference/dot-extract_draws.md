# Safely coerce MCMC samples to a numeric matrix

Safely coerce MCMC samples to a numeric matrix

## Usage

``` r
.extract_draws(
  object,
  pars = NULL,
  chains = c("stack", "first"),
  epsilon = NULL
)
```

## Arguments

- object:

  A mixgpd_fit.

- pars:

  Optional character vector of parameter names to keep (exact match).

## Value

Numeric matrix of draws (iter x parameters).

## Details

This is the matrix-oriented companion to
[`.get_samples_mcmclist()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dot-get_samples_mcmclist.md).
It can either stack all chains or keep only the first chain, always
removes stick-breaking `v` variables, applies the standard component
truncation rule, and optionally filters to an exact set of parameter
names.
