# Inverse Gaussian mixture with a GPD tail

Spliced bulk-tail family formed by attaching a generalized Pareto tail
to an inverse Gaussian mixture bulk.

## Usage

``` r
dInvGaussMixGpd(x, w, mean, shape, threshold, tail_scale, tail_shape, log = 0)

pInvGaussMixGpd(
  q,
  w,
  mean,
  shape,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rInvGaussMixGpd(n, w, mean, shape, threshold, tail_scale, tail_shape)

qInvGaussMixGpd(
  p,
  w,
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

- w:

  Numeric vector of mixture weights of length \\K\\.

- mean, shape:

  Numeric vectors of length \\K\\ giving component means and shapes.

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

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html) in quantile
  inversion.

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qInvGaussMixGpd()` returns a numeric vector with the same length as
`p`.

## Functions

- `dInvGaussMixGpd()`: Inverse Gaussian mixture + GPD tail density

- `pInvGaussMixGpd()`: Inverse Gaussian mixture + GPD tail distribution
  function

- `rInvGaussMixGpd()`: Inverse Gaussian mixture + GPD tail random
  generation

- `qInvGaussMixGpd()`: Inverse Gaussian mixture + GPD tail quantile
  function

## See also

[`InvGauss_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mix.md),
[`InvGauss_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_gpd.md),
[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`invgauss_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/invgauss_lowercase.md),
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

Other inverse-gaussian kernel families:
[`InvGauss_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_gpd.md),
[`InvGauss_mix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mix.md)

## Examples

``` r
w <- c(0.55, 0.30, 0.15)
mean <- c(1.0, 2.5, 5.0)
shape <- c(2, 4, 8)
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dInvGaussMixGpd(4.0, w = w, mean = mean, shape = shape,
               threshold = threshold, tail_scale = tail_scale,
               tail_shape = tail_shape, log = 0)
#> [1] 0.06286562
pInvGaussMixGpd(4.0, w = w, mean = mean, shape = shape,
               threshold = threshold, tail_scale = tail_scale,
               tail_shape = tail_shape, lower.tail = 1, log.p = 0)
#> [1] 0.9308478
qInvGaussMixGpd(0.50, w = w, mean = mean, shape = shape,
               threshold = threshold, tail_scale = tail_scale,
               tail_shape = tail_shape)
#> [1] 1.251694
qInvGaussMixGpd(0.95, w = w, mean = mean, shape = shape,
               threshold = threshold, tail_scale = tail_scale,
               tail_shape = tail_shape)
#> [1] 4.368537
replicate(10, rInvGaussMixGpd(1, w = w, mean = mean, shape = shape,
                             threshold = threshold,
                             tail_scale = tail_scale,
                             tail_shape = tail_shape))
#>  [1] 0.6082587 2.3634573 0.7692852 5.0918843 1.8738874 1.0267480 0.8791639
#>  [8] 0.4361379 0.5604970 3.1126706
```
