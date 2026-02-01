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
#>       x1      x2     x3
#> 1  0.474 -0.8965 -0.961
#> 2 -0.244  0.8472  0.203
#> 3 -0.934  0.7247 -1.216
#> 4  0.834  0.3055 -0.875
#> 5  0.750 -0.2327 -1.098
#> 6 -1.032  0.0937 -1.242
```
