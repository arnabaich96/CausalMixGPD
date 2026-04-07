# Extract posterior draws as a numeric matrix (iter x parameters)

Extract posterior draws as a numeric matrix (iter x parameters)

## Usage

``` r
.extract_draws_matrix(object, drop_v = TRUE, epsilon = NULL)
```

## Arguments

- object:

  A mixgpd_fit.

- drop_v:

  Logical; if TRUE, drop stick-breaking v parameters.

## Value

Numeric matrix of draws.

## Details

This helper stacks the posterior chains into one numeric matrix,
optionally removes stick-breaking `v` variables, and then applies the
component truncation rule controlled by `epsilon`. The result is the
standardized draw representation used by most internal summary and
prediction helpers.
