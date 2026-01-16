# Inverse Gaussian with a Gpd tail

Splices a generalized Pareto distribution (Gpd) above `threshold` onto a
single inverse Gaussian bulk with parameters `mean` and `shape`.

## Usage

``` r
dInvGaussGpd(x, mean, shape, threshold, tail_scale, tail_shape, log = 0)

pInvGaussGpd(
  q,
  mean,
  shape,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rInvGaussGpd(n, mean, shape, threshold, tail_scale, tail_shape)

qInvGaussGpd(
  p,
  mean,
  shape,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)
```

## Arguments

- x:

  Numeric scalar giving the point at which the density is evaluated.

- mean:

  Numeric scalar mean parameter \\\mu\>0\\.

- shape:

  Numeric scalar shape parameter \\\lambda\>0\\.

- threshold:

  Numeric scalar threshold at which the Gpd tail is attached.

- tail_scale:

  Numeric scalar Gpd scale parameter; must be positive.

- tail_shape:

  Numeric scalar Gpd shape parameter.

- log:

  Integer flag `0/1`; if `1`, return the log-density.

- q:

  Numeric scalar giving the point at which the distribution function is
  evaluated.

- lower.tail:

  Integer flag `0/1`; if `1` (default), probabilities are \\P(X \le
  q)\\.

- log.p:

  Integer flag `0/1`; if `1`, probabilities are returned on the log
  scale.

- n:

  Integer giving the number of draws. For portability inside NIMBLE, the
  RNG implementation supports `n = 1`.

- p:

  Numeric scalar probability in \\(0,1)\\ for the quantile function.

- tol:

  Numeric tolerance for numerical inversion in `qInvGaussGpd`.

- maxiter:

  Maximum iterations for numerical inversion in `qInvGaussGpd`.

## Value

Spliced density/CDF/RNG functions return numeric scalars. `qInvGaussGpd`
returns a numeric vector with the same length as `p`.

## Functions

- `dInvGaussGpd()`: Inverse Gaussian + Gpd tail density

- `pInvGaussGpd()`: Inverse Gaussian + Gpd tail distribution function

- `rInvGaussGpd()`: Inverse Gaussian + Gpd tail random generation

- `qInvGaussGpd()`: Inverse Gaussian + Gpd tail quantile function

## Examples

``` r
mean <- 2.5
shape <- 6
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dInvGaussGpd(4.0, mean = mean, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape, log = 0)
#> [1] 0.09181898
pInvGaussGpd(4.0, mean = mean, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape, lower.tail = 1, log.p = 0)
#> [1] 0.8989991
qInvGaussGpd(0.50, mean = mean, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape)
#> [1] 2.077493
qInvGaussGpd(0.95, mean = mean, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape)
#> [1] 4.830437
replicate(10, rInvGaussGpd(1, mean = mean, shape = shape,
                          threshold = threshold,
                          tail_scale = tail_scale,
                          tail_shape = tail_shape))
#>  [1] 3.012041 3.470115 3.186169 2.954170 2.101301 6.317948 1.734722 5.551019
#>  [9] 1.645503 1.122363
```
