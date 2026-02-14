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
#>           x1         x2         x3        x4
#> 1  0.8619272  0.5179205 -0.6660152 0.1608057
#> 2 -0.1445059 -0.4322996 -0.1059911 0.8932175
#> 3 -0.5665718 -0.1223736 -1.1039574 0.5830409
#> 4 -0.9501948  0.1356688 -0.5949445 0.3672704
#> 5  0.3625201 -0.1830098  0.7159558 0.1416564
#> 6  0.7009523 -0.5394179  0.9738608 0.9295270
```
