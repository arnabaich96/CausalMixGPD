# nc_real200_k2 dataset

Real-line, bulk-only mixture dataset with K=2 components and no
covariates. Intended for non-causal bulk-only vignettes
(normal/laplace/cauchy, GPD=FALSE).

## Usage

``` r
nc_real200_k2
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
data("nc_real200_k2")
head(nc_real200_k2$y)
#> [1] -1.7079605 -0.5634694 -2.2334859  1.8734314  1.4309543 -0.3023361
```
