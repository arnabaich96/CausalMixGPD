# Lowercase vectorized Gamma distribution functions

Vectorized R wrappers for Gamma mixture, Gamma mixture + GPD, and
Gamma + GPD distribution functions. These lowercase versions accept
vector inputs for the first argument (`x`, `q`, or `p`) and return a
numeric vector. The `r*` functions support `n > 1`.

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

## Examples

``` r
w <- c(0.55, 0.3, 0.15)
shp <- c(2, 4, 6)
scl <- c(1, 2.5, 5)

# Gamma mixture
dgammamix(c(1, 2, 3), w = w, shape = shp, scale = scl)
#> [1] 0.2032 0.1535 0.0926
rgammamix(5, w = w, shape = shp, scale = scl)
#> [1]  5.249 13.925  0.463 10.743 34.572

# Gamma mixture + GPD
dgammamixgpd(c(2, 3, 4), w = w, shape = shp, scale = scl,
             threshold = 3, tail_scale = 0.9, tail_shape = 0.2)
#> [1] 0.153 0.610 0.183

# Gamma + GPD (single component)
dgammagpd(c(2, 3, 4), shape = 4, scale = 2.5, threshold = 3,
          tail_scale = 0.9, tail_shape = 0.2)
#> [1] 0.0153 1.0736 0.3221
```
