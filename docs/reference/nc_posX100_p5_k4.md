# nc_posX100_p5_k4 dataset

Positive-support dataset with covariates (p=5) and K=4 mixture
components. Intended for covariate and prediction vignettes (GPD=FALSE).

## Usage

``` r
nc_posX100_p5_k4
```

## Format

A list with:

- y:

  Numeric outcome vector.

- X:

  data.frame with x1-x5.

- meta:

  List with n, support, p, K_true, tail, exceed_frac, seed.

- truth:

  List with kernel, weights, params, threshold, tail_params.

## Examples

``` r
data("nc_posX100_p5_k4")
head(nc_posX100_p5_k4$X)
#>       x1     x2     x3     x4     x5
#> 1  0.784  0.959  0.241 0.2430  2.326
#> 2 -1.199  0.675  1.532 0.7289  0.935
#> 3 -0.883 -0.116  0.283 0.3253 -1.370
#> 4 -1.025  0.616  0.976 0.0929  0.886
#> 5  1.849 -0.652 -0.924 0.3627 -0.582
#> 6  1.207 -0.581  0.571 0.8203  1.316
```
