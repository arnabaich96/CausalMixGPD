# nc_realX100_p5_k3 dataset

Real-line dataset with covariates (p=5) and K=3 mixture components.
Intended for covariate and prediction vignettes (GPD=FALSE).

## Usage

``` r
nc_realX100_p5_k3
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
data("nc_realX100_p5_k3")
head(nc_realX100_p5_k3$X)
#>           x1         x2          x3         x4           x5
#> 1 -1.1678910  0.1267816  0.15803426 0.02734277  0.006980795
#> 2  0.3432765 -0.0726234 -0.07397735 0.44207926  2.183304592
#> 3  0.4370006 -0.1847439 -0.86390792 0.87718039 -0.685905507
#> 4  2.4631722  0.5690263  0.62977622 0.88681904  1.836210030
#> 5 -0.9804281  0.2318589  0.76525738 0.42147125  0.932465382
#> 6 -0.5428538  0.3863705  1.27017473 0.83929084  0.035313791
```
