# Cauchy mixture distribution

A finite mixture of Cauchy components. Base Cauchy functions are taken
from stats. The mixture density and distribution function are computed
by weighted sums. Random generation samples a component index according
to the weights and draws from the corresponding component. Quantiles are
computed by numerical inversion of the mixture CDF. The `d*`, `p*`, and
`q*` functions accept vector inputs for their first argument and
evaluate elementwise; `r*` supports `n > 1`.

## Usage

``` r
dCauchyMix(x, w, location, scale, log = 0)

pCauchyMix(q, w, location, scale, lower.tail = 1, log.p = 0)

rCauchyMix(n, w, location, scale)

qCauchyMix(
  p,
  w,
  location,
  scale,
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

  Numeric vector of mixture weights of length \\K\\. The functions
  normalize `w` internally when needed.

- location, scale:

  Numeric vectors of length \\K\\ giving component locations and scales.

- log:

  Logical; if `TRUE`, return the log-density.

- q:

  Numeric scalar giving the point at which the distribution function is
  evaluated.

- lower.tail:

  Logical; if `TRUE` (default), probabilities are \\P(X \le q)\\.

- log.p:

  Logical; if `TRUE`, probabilities are returned on the log scale.

- n:

  Integer giving the number of draws. The RNG implementation supports
  `n = 1`.

- p:

  Numeric scalar probability in \\(0,1)\\ for the quantile function.

- tol:

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Density/CDF/RNG functions return numeric scalars. `qCauchyMix` returns a
numeric vector with the same length as `p`.

## Functions

- `dCauchyMix()`: Cauchy mixture density

- `pCauchyMix()`: Cauchy mixture distribution function

- `rCauchyMix()`: Cauchy mixture random generation

- `qCauchyMix()`: Cauchy mixture quantile function

## Examples

``` r
w <- c(0.50, 0.30, 0.20)
location <- c(-2, 0, 3)
scale <- c(1.0, 0.7, 1.5)

dCauchyMix(0.5, w = w, location = location, scale = scale, log = FALSE)
#> [1] 0.124
pCauchyMix(0.5, w = w, location = location, scale = scale,
           lower.tail = TRUE, log.p = FALSE)
#> [1] 0.683
qCauchyMix(0.50, w = w, location = location, scale = scale)
#> [1] -0.664
qCauchyMix(0.95, w = w, location = location, scale = scale)
#> [1] 7
replicate(10, rCauchyMix(1, w = w, location = location, scale = scale))
#>  [1] -0.846 -0.862 -2.580 -0.690  1.779  5.349  3.392 45.684 -0.874 -1.163
```
