# nc_realX100_p3_k2 dataset

Real-line dataset with covariates (p=3) and K=2 mixture components.
Intended for covariate and prediction vignettes (GPD=FALSE).

## Usage

``` r
nc_realX100_p3_k2
```

## Format

A list with:

- y:

  Numeric outcome vector.

- X:

  data.frame with x1-x3.

- meta:

  List with n, support, p, K_true, tail, exceed_frac, seed.

- truth:

  List with kernel, weights, params, threshold, tail_params.

## Examples

``` r
data("nc_realX100_p3_k2")
head(nc_realX100_p3_k2$X)
#>       x1      x2     x3
#> 1  0.497 -0.8024  1.666
#> 2 -1.494  0.2360 -1.227
#> 3 -0.555  0.0475  0.554
#> 4 -0.215  0.8879  1.050
#> 5 -1.200 -0.9009 -0.440
#> 6  1.615  0.3328  0.465
```
