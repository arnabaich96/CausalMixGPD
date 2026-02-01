# Lowercase vectorized Laplace distribution functions

Vectorized R wrappers for Laplace mixture, Laplace mixture + GPD, and
Laplace + GPD distribution functions. These lowercase versions accept
vector inputs for the first argument (`x`, `q`, or `p`) and return a
numeric vector. The `r*` functions support `n > 1`.

## Usage

``` r
dlaplacemix(x, w, location, scale, log = FALSE)

plaplacemix(q, w, location, scale, lower.tail = TRUE, log.p = FALSE)

qlaplacemix(
  p,
  w,
  location,
  scale,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rlaplacemix(n, w, location, scale)

dlaplacemixgpd(
  x,
  w,
  location,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  log = FALSE
)

plaplacemixgpd(
  q,
  w,
  location,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qlaplacemixgpd(
  p,
  w,
  location,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rlaplacemixgpd(n, w, location, scale, threshold, tail_scale, tail_shape)

dlaplacegpd(x, location, scale, threshold, tail_scale, tail_shape, log = FALSE)

plaplacegpd(
  q,
  location,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qlaplacegpd(
  p,
  location,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

rlaplacegpd(n, location, scale, threshold, tail_scale, tail_shape)
```

## Arguments

- x:

  Numeric vector of quantiles.

- w:

  Numeric vector of mixture weights.

- location, scale:

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

- `dlaplacemix()`: Laplace mixture density (vectorized)

- `plaplacemix()`: Laplace mixture distribution function (vectorized)

- `qlaplacemix()`: Laplace mixture quantile function (vectorized)

- `rlaplacemix()`: Laplace mixture random generation (vectorized)

- `dlaplacemixgpd()`: Laplace mixture + GPD density (vectorized)

- `plaplacemixgpd()`: Laplace mixture + GPD distribution function
  (vectorized)

- `qlaplacemixgpd()`: Laplace mixture + GPD quantile function
  (vectorized)

- `rlaplacemixgpd()`: Laplace mixture + GPD random generation
  (vectorized)

- `dlaplacegpd()`: Laplace + GPD density (vectorized)

- `plaplacegpd()`: Laplace + GPD distribution function (vectorized)

- `qlaplacegpd()`: Laplace + GPD quantile function (vectorized)

- `rlaplacegpd()`: Laplace + GPD random generation (vectorized)

## Examples

``` r
w <- c(0.6, 0.3, 0.1)
loc <- c(0, 1, -2)
scl <- c(1, 0.9, 1.1)

# Laplace mixture
dlaplacemix(c(-1, 0, 1), w = w, location = loc, scale = scl)
#> [1] 0.147 0.362 0.280
rlaplacemix(5, w = w, location = loc, scale = scl)
#> [1]  0.190  1.633 -0.753 -0.372  1.501
```
