# nc_posX100_p3_k2 dataset

Positive-support dataset with covariates (p=3) and K=2 mixture
components. Intended for covariate and prediction vignettes (GPD=FALSE).

## Usage

``` r
nc_posX100_p3_k2
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
data("nc_posX100_p3_k2")
head(nc_posX100_p3_k2$X)
#>           x1          x2         x3
#> 1  0.4736636 -0.89650447 -0.9613510
#> 2 -0.2440838  0.84724154  0.2025749
#> 3 -0.9342857  0.72466830 -1.2161319
#> 4  0.8338413  0.30551457 -0.8753508
#> 5  0.7502107 -0.23269967 -1.0976574
#> 6 -1.0316232  0.09371796 -1.2415369
```
