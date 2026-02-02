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
#>       x1      x2     x3     x4       x5
#> 1 -1.168  0.1268  0.158 0.0273  0.00698
#> 2  0.343 -0.0726 -0.074 0.4421  2.18330
#> 3  0.437 -0.1847 -0.864 0.8772 -0.68591
#> 4  2.463  0.5690  0.630 0.8868  1.83621
#> 5 -0.980  0.2319  0.765 0.4215  0.93247
#> 6 -0.543  0.3864  1.270 0.8393  0.03531
```
