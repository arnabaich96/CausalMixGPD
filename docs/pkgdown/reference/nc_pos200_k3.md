# nc_pos200_k3 dataset

Positive-support, bulk-only mixture dataset with K=3 components and no
covariates. Intended for non-causal bulk-only positive-kernel vignettes
(gamma/lognormal/invgauss/amoroso).

## Usage

``` r
nc_pos200_k3
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
data("nc_pos200_k3")
head(nc_pos200_k3$y)
#> [1] 0.6453111 2.8722793 9.8424691 3.1839504 7.2954021 7.0877631
```
