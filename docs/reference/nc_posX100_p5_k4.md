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
#>           x1         x2         x3        x4         x5
#> 1  0.7840847  0.9588580  0.2413081 0.2430204  2.3259137
#> 2 -1.1988719  0.6747353  1.5319196 0.7289287  0.9350308
#> 3 -0.8832436 -0.1164747  0.2825188 0.3252745 -1.3697156
#> 4 -1.0246299  0.6164253  0.9757708 0.0928507  0.8859178
#> 5  1.8490863 -0.6522175 -0.9235545 0.3627036 -0.5817533
#> 6  1.2072248 -0.5812129  0.5708284 0.8202687  1.3157071
```
