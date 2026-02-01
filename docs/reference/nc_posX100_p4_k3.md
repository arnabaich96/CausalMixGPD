# nc_posX100_p4_k3 dataset

Positive-support dataset with covariates (p=4) and K=3 mixture
components. Intended for covariate and prediction vignettes (GPD=FALSE).

## Usage

``` r
nc_posX100_p4_k3
```

## Format

A list with:

- y:

  Numeric outcome vector.

- X:

  data.frame with x1-x4.

- meta:

  List with n, support, p, K_true, tail, exceed_frac, seed.

- truth:

  List with kernel, weights, params, threshold, tail_params.

## Examples

``` r
data("nc_posX100_p4_k3")
head(nc_posX100_p4_k3$X)
#>       x1     x2     x3    x4
#> 1  0.862  0.518 -0.666 0.161
#> 2 -0.145 -0.432 -0.106 0.893
#> 3 -0.567 -0.122 -1.104 0.583
#> 4 -0.950  0.136 -0.595 0.367
#> 5  0.363 -0.183  0.716 0.142
#> 6  0.701 -0.539  0.974 0.930
```
