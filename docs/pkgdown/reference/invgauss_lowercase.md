# Lowercase vectorized inverse Gaussian distribution functions

Vectorized R wrappers for the scalar inverse-Gaussian-kernel topics in
this file.

## Usage

``` r
dinvgaussmix(x, w, mean, shape, log = FALSE)

pinvgaussmix(q, w, mean, shape, lower.tail = TRUE, log.p = FALSE)

qinvgaussmix(
  p,
  w,
  mean,
  shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rinvgaussmix(n, w, mean, shape)

dinvgaussmixgpd(
  x,
  w,
  mean,
  shape,
  threshold,
  tail_scale,
  tail_shape,
  log = FALSE
)

pinvgaussmixgpd(
  q,
  w,
  mean,
  shape,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qinvgaussmixgpd(
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

rinvgaussmixgpd(n, w, mean, shape, threshold, tail_scale, tail_shape)

dinvgaussgpd(x, mean, shape, threshold, tail_scale, tail_shape, log = FALSE)

pinvgaussgpd(
  q,
  mean,
  shape,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qinvgaussgpd(
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

rinvgaussgpd(n, mean, shape, threshold, tail_scale, tail_shape)
```

## Arguments

- x:

  Numeric vector of quantiles.

- w:

  Numeric vector of mixture weights.

- mean, shape:

  Numeric vectors (mix) or scalars (base+gpd) of component parameters.

- log:

  Logical; if `TRUE`, return log-density.

- q:

  Numeric vector of quantiles.

- lower.tail:

  Logical; if `TRUE` (default), probabilities are \\P(X \le x)\\.

- log.p:

  Logical; if `TRUE`, probabilities are on log scale.

- p:

  Numeric vector of probabilities.

- tol, maxiter:

  Tolerance and max iterations for numerical inversion.

- n:

  Integer number of observations to generate.

- threshold, tail_scale, tail_shape:

  GPD tail parameters (scalars).

## Value

Numeric vector of densities, probabilities, quantiles, or random
variates.

## Details

These functions are vectorized R front ends to the scalar
inverse-Gaussian and splice routines. They retain the \\(\mu,\lambda)\\
parameterization used everywhere else in the package and simply apply
the scalar evaluator repeatedly over the supplied input vector or draw
index.

## Functions

- `dinvgaussmix()`: Inverse Gaussian mixture density (vectorized)

- `pinvgaussmix()`: Inverse Gaussian mixture distribution function
  (vectorized)

- `qinvgaussmix()`: Inverse Gaussian mixture quantile function
  (vectorized)

- `rinvgaussmix()`: Inverse Gaussian mixture random generation
  (vectorized)

- `dinvgaussmixgpd()`: Inverse Gaussian mixture + GPD density
  (vectorized)

- `pinvgaussmixgpd()`: Inverse Gaussian mixture + GPD distribution
  function (vectorized)

- `qinvgaussmixgpd()`: Inverse Gaussian mixture + GPD quantile function
  (vectorized)

- `rinvgaussmixgpd()`: Inverse Gaussian mixture + GPD random generation
  (vectorized)

- `dinvgaussgpd()`: Inverse Gaussian + GPD density (vectorized)

- `pinvgaussgpd()`: Inverse Gaussian + GPD distribution function
  (vectorized)

- `qinvgaussgpd()`: Inverse Gaussian + GPD quantile function
  (vectorized)

- `rinvgaussgpd()`: Inverse Gaussian + GPD random generation
  (vectorized)

## See also

[`InvGauss_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mix.md),
[`InvGauss_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mixgpd.md),
[`InvGauss_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_gpd.md),
[`bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`get_kernel_registry()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/get_kernel_registry.md).

Other vectorized kernel helpers:
[`amoroso_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_lowercase.md),
[`base_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md),
[`cauchy_mix_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy_mix_lowercase.md),
[`gamma_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md),
[`laplace_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md),
[`lognormal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_lowercase.md),
[`normal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_lowercase.md)

## Examples

``` r
w <- c(0.6, 0.3, 0.1)
mu <- c(1, 1.5, 2)
lam <- c(2, 3, 4)

# Inverse Gaussian mixture
dinvgaussmix(c(1, 2, 3), w = w, mean = mu, shape = lam)
#> [1] 0.56238059 0.16823110 0.05436761
rinvgaussmix(5, w = w, mean = mu, shape = lam)
#> [1] 0.9804490 0.7447751 0.6244059 0.8138030 1.2787016
```
