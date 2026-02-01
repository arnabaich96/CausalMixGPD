# nc_pos_tail200_k4 dataset

Positive-support, tail-designed mixture dataset with K=4 components and
no covariates. Intended for GPD vignettes
(gamma/lognormal/invgauss/amoroso with GPD=TRUE).

## Usage

``` r
nc_pos_tail200_k4
```

## Format

A list with:

- y:

  Numeric outcome vector.

- X:

  NULL.

- meta:

  List with n, support, p, K_true, tail, exceed_frac, seed.

- truth:

  List with kernel, weights, params, threshold, tail_params.

## Examples

``` r
data("nc_pos_tail200_k4")
head(nc_pos_tail200_k4$y)
#> [1] 0.744 1.267 2.284 0.605 0.941 1.579
```
