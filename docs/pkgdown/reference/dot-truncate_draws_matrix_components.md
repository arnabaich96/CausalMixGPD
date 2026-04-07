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

## Details

Posterior draw matrices often contain more components than are
effectively needed for reporting. This helper applies the package
truncation rule draw-by-draw, keeping the retained component blocks,
associated weights, and any linked coefficient matrices aligned after
reordering and truncation.

The bookkeeping attached to the returned matrix records both the
cumulative mass rule and the per-component weight rule so later
summaries can report how many components were effectively retained.
