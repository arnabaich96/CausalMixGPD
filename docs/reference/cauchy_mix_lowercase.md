# Lowercase vectorized Cauchy mixture distribution functions

Vectorized R wrappers for Cauchy mixture distribution functions. These
lowercase versions accept vector inputs for the first argument (`x`,
`q`, or `p`) and return a numeric vector. The `r*` functions support
`n > 1`.

## Usage

``` r
dcauchymix(x, w, location, scale, log = FALSE)

pcauchymix(q, w, location, scale, lower.tail = TRUE, log.p = FALSE)

qcauchymix(
  p,
  w,
  location,
  scale,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rcauchymix(n, w, location, scale)
```

## Arguments

- x:

  Numeric vector of quantiles.

- w:

  Numeric vector of mixture weights.

- location, scale:

  Numeric vectors of component parameters.

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

## Value

Numeric vector of densities, probabilities, quantiles, or random
variates.

## Functions

- `dcauchymix()`: Cauchy mixture density (vectorized)

- `pcauchymix()`: Cauchy mixture distribution function (vectorized)

- `qcauchymix()`: Cauchy mixture quantile function (vectorized)

- `rcauchymix()`: Cauchy mixture random generation (vectorized)

## Examples

``` r
w <- c(0.6, 0.3, 0.1)
loc <- c(-1, 0, 1)
scl <- c(1, 1.2, 2)

dcauchymix(c(-2, 0, 2), w = w, location = loc, scale = scl)
#> [1] 0.1215 0.1878 0.0529
rcauchymix(5, w = w, location = loc, scale = scl)
#> [1]  4.5762  0.0038 -0.3569 -0.0722 -4.3743
```
