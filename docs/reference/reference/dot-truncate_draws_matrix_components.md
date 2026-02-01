# Truncate component draws in a draws matrix

Truncate component draws in a draws matrix

## Usage

``` r
.truncate_draws_matrix_components(object, mat, epsilon)
```

## Arguments

- object:

  A mixgpd_fit object.

- mat:

  Numeric matrix of draws (iter x parameters).

- epsilon:

  Numeric in \[0,1). Truncation level.

## Value

Numeric matrix with truncated components.
