# Lowercase vectorized Amoroso distribution functions

Vectorized R wrappers for Amoroso mixture, Amoroso mixture + GPD, and
Amoroso + GPD distribution functions. These lowercase versions accept
vector inputs for the first argument (`x`, `q`, or `p`) and return a
numeric vector. The `r*` functions support `n > 1`.

## Usage

``` r
damorosomix(x, w, loc, scale, shape1, shape2, log = FALSE)

pamorosomix(q, w, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE)

qamorosomix(
  p,
  w,
  loc,
  scale,
  shape1,
  shape2,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

ramorosomix(n, w, loc, scale, shape1, shape2)

damorosomixgpd(
  x,
  w,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape,
  log = FALSE
)

pamorosomixgpd(
  q,
  w,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qamorosomixgpd(
  p,
  w,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

ramorosomixgpd(
  n,
  w,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape
)

damorosogpd(
  x,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape,
  log = FALSE
)

pamorosogpd(
  q,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qamorosogpd(
  p,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

ramorosogpd(n, loc, scale, shape1, shape2, threshold, tail_scale, tail_shape)
```

## Arguments

- x:

  Numeric vector of quantiles.

- w:

  Numeric vector of mixture weights.

- loc, scale, shape1, shape2:

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

- `damorosomix()`: Amoroso mixture density (vectorized)

- `pamorosomix()`: Amoroso mixture distribution function (vectorized)

- `qamorosomix()`: Amoroso mixture quantile function (vectorized)

- `ramorosomix()`: Amoroso mixture random generation (vectorized)

- `damorosomixgpd()`: Amoroso mixture + GPD density (vectorized)

- `pamorosomixgpd()`: Amoroso mixture + GPD distribution function
  (vectorized)

- `qamorosomixgpd()`: Amoroso mixture + GPD quantile function
  (vectorized)

- `ramorosomixgpd()`: Amoroso mixture + GPD random generation
  (vectorized)

- `damorosogpd()`: Amoroso + GPD density (vectorized)

- `pamorosogpd()`: Amoroso + GPD distribution function (vectorized)

- `qamorosogpd()`: Amoroso + GPD quantile function (vectorized)

- `ramorosogpd()`: Amoroso + GPD random generation (vectorized)

## Examples

``` r
w <- c(0.6, 0.3, 0.1)
locs <- c(0.5, 0.5, 0.5)
scls <- c(1, 1.3, 1.6)
s1 <- c(2.5, 3, 4)
s2 <- c(1.2, 1.2, 1.2)

# Amoroso mixture
damorosomix(c(1, 2, 3), w = w, loc = locs, scale = scls, shape1 = s1, shape2 = s2)
#> Error in damorosomix(c(1, 2, 3), w = w, loc = locs, scale = scls, shape1 = s1,     shape2 = s2): could not find function "damorosomix"
ramorosomix(5, w = w, loc = locs, scale = scls, shape1 = s1, shape2 = s2)
#> Error in ramorosomix(5, w = w, loc = locs, scale = scls, shape1 = s1,     shape2 = s2): could not find function "ramorosomix"
```
