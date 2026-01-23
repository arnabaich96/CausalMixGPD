# Lowercase vectorized Lognormal distribution functions

Vectorized R wrappers for Lognormal mixture, Lognormal mixture + GPD,
and Lognormal + GPD distribution functions. These lowercase versions
accept vector inputs for the first argument (`x`, `q`, or `p`) and
return a numeric vector. The `r*` functions support `n > 1`.

## Usage

``` r
dlognormalmix(x, w, meanlog, sdlog, log = FALSE)

plognormalmix(q, w, meanlog, sdlog, lower.tail = TRUE, log.p = FALSE)

qlognormalmix(
  p,
  w,
  meanlog,
  sdlog,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rlognormalmix(n, w, meanlog, sdlog)

dlognormalmixgpd(
  x,
  w,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  log = FALSE
)

plognormalmixgpd(
  q,
  w,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qlognormalmixgpd(
  p,
  w,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rlognormalmixgpd(n, w, meanlog, sdlog, threshold, tail_scale, tail_shape)

dlognormalgpd(
  x,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  log = FALSE
)

plognormalgpd(
  q,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qlognormalgpd(
  p,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

rlognormalgpd(n, meanlog, sdlog, threshold, tail_scale, tail_shape)
```

## Arguments

- x:

  Numeric vector of quantiles.

- w:

  Numeric vector of mixture weights.

- meanlog, sdlog:

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

- `dlognormalmix()`: Lognormal mixture density (vectorized)

- `plognormalmix()`: Lognormal mixture distribution function
  (vectorized)

- `qlognormalmix()`: Lognormal mixture quantile function (vectorized)

- `rlognormalmix()`: Lognormal mixture random generation (vectorized)

- `dlognormalmixgpd()`: Lognormal mixture + GPD density (vectorized)

- `plognormalmixgpd()`: Lognormal mixture + GPD distribution function
  (vectorized)

- `qlognormalmixgpd()`: Lognormal mixture + GPD quantile function
  (vectorized)

- `rlognormalmixgpd()`: Lognormal mixture + GPD random generation
  (vectorized)

- `dlognormalgpd()`: Lognormal + GPD density (vectorized)

- `plognormalgpd()`: Lognormal + GPD distribution function (vectorized)

- `qlognormalgpd()`: Lognormal + GPD quantile function (vectorized)

- `rlognormalgpd()`: Lognormal + GPD random generation (vectorized)

## Examples

``` r
w <- c(0.6, 0.3, 0.1)
ml <- c(0, 0.3, 0.6)
sl <- c(0.4, 0.5, 0.6)

# Lognormal mixture
dlognormalmix(c(1, 2, 3), w = w, meanlog = ml, sdlog = sl)
#> [1] 0.8387 0.1874 0.0426
rlognormalmix(5, w = w, meanlog = ml, sdlog = sl)
#> [1] 0.636 0.756 1.583 1.456 0.573

# Lognormal mixture + GPD
dlognormalmixgpd(c(2, 3, 4), w = w, meanlog = ml, sdlog = sl,
                 threshold = 2.5, tail_scale = 0.5, tail_shape = 0.2)
#> [1] 0.18737 0.04632 0.00824
```
