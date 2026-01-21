# Lowercase vectorized Normal distribution functions

Vectorized R wrappers for Normal mixture, Normal mixture + GPD, and
Normal + GPD distribution functions. These lowercase versions accept
vector inputs for the first argument (`x`, `q`, or `p`) and return a
numeric vector. The `r*` functions support `n > 1`.

## Usage

``` r
dnormmix(x, w, mean, sd, log = FALSE)

pnormmix(q, w, mean, sd, lower.tail = TRUE, log.p = FALSE)

qnormmix(
  p,
  w,
  mean,
  sd,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rnormmix(n, w, mean, sd)

dnormmixgpd(x, w, mean, sd, threshold, tail_scale, tail_shape, log = FALSE)

pnormmixgpd(
  q,
  w,
  mean,
  sd,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qnormmixgpd(
  p,
  w,
  mean,
  sd,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rnormmixgpd(n, w, mean, sd, threshold, tail_scale, tail_shape)

dnormgpd(x, mean, sd, threshold, tail_scale, tail_shape, log = FALSE)

pnormgpd(
  q,
  mean,
  sd,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qnormgpd(
  p,
  mean,
  sd,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

rnormgpd(n, mean, sd, threshold, tail_scale, tail_shape)
```

## Arguments

- x:

  Numeric vector of quantiles.

- w:

  Numeric vector of mixture weights.

- mean, sd:

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

- `dnormmix()`: Normal mixture density (vectorized)

- `pnormmix()`: Normal mixture distribution function (vectorized)

- `qnormmix()`: Normal mixture quantile function (vectorized)

- `rnormmix()`: Normal mixture random generation (vectorized)

- `dnormmixgpd()`: Normal mixture + GPD density (vectorized)

- `pnormmixgpd()`: Normal mixture + GPD distribution function
  (vectorized)

- `qnormmixgpd()`: Normal mixture + GPD quantile function (vectorized)

- `rnormmixgpd()`: Normal mixture + GPD random generation (vectorized)

- `dnormgpd()`: Normal + GPD density (vectorized)

- `pnormgpd()`: Normal + GPD distribution function (vectorized)

- `qnormgpd()`: Normal + GPD quantile function (vectorized)

- `rnormgpd()`: Normal + GPD random generation (vectorized)

## Examples

``` r
w <- c(0.6, 0.25, 0.15)
mu <- c(-1, 0.5, 2)
sig <- c(1, 0.7, 1.3)

# Normal mixture
dnormmix(c(0, 1, 2), w = w, mean = mu, sd = sig)
#> Error in dnormmix(c(0, 1, 2), w = w, mean = mu, sd = sig): could not find function "dnormmix"
rnormmix(5, w = w, mean = mu, sd = sig)
#> Error in rnormmix(5, w = w, mean = mu, sd = sig): could not find function "rnormmix"

# Normal mixture + GPD
dnormmixgpd(c(1, 2, 3), w = w, mean = mu, sd = sig,
            threshold = 2, tail_scale = 1, tail_shape = 0.2)
#> Error in dnormmixgpd(c(1, 2, 3), w = w, mean = mu, sd = sig, threshold = 2,     tail_scale = 1, tail_shape = 0.2): could not find function "dnormmixgpd"

# Normal + GPD (single component)
dnormgpd(c(1, 2, 3), mean = 0.5, sd = 1, threshold = 2,
         tail_scale = 1, tail_shape = 0.2)
#> Error in dnormgpd(c(1, 2, 3), mean = 0.5, sd = 1, threshold = 2, tail_scale = 1,     tail_shape = 0.2): could not find function "dnormgpd"
```
