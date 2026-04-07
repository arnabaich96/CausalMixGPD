# Lowercase vectorized gamma distribution functions

Vectorized R wrappers for the scalar gamma-kernel topics in this file.

## Usage

``` r
dgammamix(x, w, shape, scale, log = FALSE)

pgammamix(q, w, shape, scale, lower.tail = TRUE, log.p = FALSE)

qgammamix(
  p,
  w,
  shape,
  scale,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rgammamix(n, w, shape, scale)

dgammamixgpd(
  x,
  w,
  shape,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  log = FALSE
)

pgammamixgpd(
  q,
  w,
  shape,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qgammamixgpd(
  p,
  w,
  shape,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rgammamixgpd(n, w, shape, scale, threshold, tail_scale, tail_shape)

dgammagpd(x, shape, scale, threshold, tail_scale, tail_shape, log = FALSE)

pgammagpd(
  q,
  shape,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qgammagpd(
  p,
  shape,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

rgammagpd(n, shape, scale, threshold, tail_scale, tail_shape)
```

## Arguments

- x:

  Numeric vector of quantiles.

- w:

  Numeric vector of mixture weights.

- shape, scale:

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

These wrappers are vectorized interfaces to the scalar gamma and
gamma-plus-GPD routines. They preserve the package's shape-scale
parameterization and the same splice definition used in the fitted-model
prediction code. Quantile wrappers delegate to the scalar inversion code
rather than implementing separate approximations.

## Functions

- `dgammamix()`: Gamma mixture density (vectorized)

- `pgammamix()`: Gamma mixture distribution function (vectorized)

- `qgammamix()`: Gamma mixture quantile function (vectorized)

- `rgammamix()`: Gamma mixture random generation (vectorized)

- `dgammamixgpd()`: Gamma mixture + GPD density (vectorized)

- `pgammamixgpd()`: Gamma mixture + GPD distribution function
  (vectorized)

- `qgammamixgpd()`: Gamma mixture + GPD quantile function (vectorized)

- `rgammamixgpd()`: Gamma mixture + GPD random generation (vectorized)

- `dgammagpd()`: Gamma + GPD density (vectorized)

- `pgammagpd()`: Gamma + GPD distribution function (vectorized)

- `qgammagpd()`: Gamma + GPD quantile function (vectorized)

- `rgammagpd()`: Gamma + GPD random generation (vectorized)

## See also

[`gamma_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mix.md),
[`gamma_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mixgpd.md),
[`gamma_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_gpd.md),
[`bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`get_kernel_registry()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/get_kernel_registry.md).

Other vectorized kernel helpers:
[`amoroso_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_lowercase.md),
[`base_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md),
[`cauchy_mix_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy_mix_lowercase.md),
[`invgauss_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/invgauss_lowercase.md),
[`laplace_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md),
[`lognormal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_lowercase.md),
[`normal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_lowercase.md)

## Examples

``` r
w <- c(0.55, 0.3, 0.15)
shp <- c(2, 4, 6)
scl <- c(1, 2.5, 5)

# Gamma mixture
dgammamix(c(1, 2, 3), w = w, shape = shp, scale = scl)
#> [1] 0.2031918 0.1534717 0.0925686
rgammamix(5, w = w, shape = shp, scale = scl)
#> [1]  2.032069  6.805664 58.093898  7.487449  7.682572

# Gamma mixture + GPD
dgammamixgpd(c(2, 3, 4), w = w, shape = shp, scale = scl,
             threshold = 3, tail_scale = 0.9, tail_shape = 0.2)
#> [1] 0.1534717 0.6104389 0.1831223

# Gamma + GPD (single component)
dgammagpd(c(2, 3, 4), shape = 4, scale = 2.5, threshold = 3,
          tail_scale = 0.9, tail_shape = 0.2)
#> [1] 0.0153371 1.0735900 0.3220605
```
