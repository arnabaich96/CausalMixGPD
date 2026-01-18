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
#>           x1          x2         x3
#> 1  0.4971905 -0.80242631  1.6658444
#> 2 -1.4938180  0.23603870 -1.2269103
#> 3 -0.5548875  0.04752863  0.5536624
#> 4 -0.2150968  0.88790705  1.0502246
#> 5 -1.1997984 -0.90089612 -0.4397667
#> 6  1.6147807  0.33279527  0.4649012
```
