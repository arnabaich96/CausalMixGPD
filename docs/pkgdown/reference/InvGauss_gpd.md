# Inverse Gaussian with a GPD tail

Spliced family obtained by attaching a generalized Pareto tail above
`threshold` to a single inverse Gaussian bulk.

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

  Numeric scalar threshold at which the GPD tail is attached.

- tail_scale:

  Numeric scalar GPD scale parameter; must be positive.

- tail_shape:

  Numeric scalar GPD shape parameter.

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

Spliced density/CDF/RNG functions return numeric scalars.
`qInvGaussGpd()` returns a numeric vector with the same length as `p`.

## Functions

- `dInvGaussGpd()`: Inverse Gaussian + GPD tail density

- `pInvGaussGpd()`: Inverse Gaussian + GPD tail distribution function

- `rInvGaussGpd()`: Inverse Gaussian + GPD tail random generation

- `qInvGaussGpd()`: Inverse Gaussian + GPD tail quantile function

## See also

[`InvGauss_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mix.md),
[`InvGauss_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mixgpd.md),
[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`invgauss_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/invgauss_lowercase.md).

Other inverse-gaussian kernel families:
[`InvGauss_mix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mix.md),
[`InvGauss_mixgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mixgpd.md)

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
#>  [1] 1.2118394 0.9678417 8.0586675 2.7613902 4.8990879 1.2286165 1.4783701
#>  [8] 7.1150727 2.7240247 5.2946966
```
