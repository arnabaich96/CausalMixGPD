# Laplace (double exponential) mixture distribution

A finite mixture of Laplace (double exponential) components for
real-valued data. Base Laplace functions are taken from nimble (`ddexp`,
`pdexp`, `rdexp`, `qdexp`).

## Usage

``` r
dLaplaceMix(x, w, location, scale, log = 0)

pLaplaceMix(q, w, location, scale, lower.tail = 1, log.p = 0)

rLaplaceMix(n, w, location, scale)

qLaplaceMix(
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

- location:

  Numeric vector of length \\K\\ giving component locations.

- scale:

  Numeric vector of length \\K\\ giving component scales.

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

  Integer maximum iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Density/CDF/RNG functions return numeric scalars. `qLaplaceMix` returns
a numeric vector with the same length as `p`.

## Details

Mixture density and CDF are computed by weighted sums. Random generation
samples a component index according to weights and draws from the
corresponding component. Quantiles are computed by numerical inversion
of the mixture CDF.

## Functions

- `dLaplaceMix()`: Laplace mixture density

- `pLaplaceMix()`: Laplace mixture distribution function

- `rLaplaceMix()`: Laplace mixture random generation

- `qLaplaceMix()`: Laplace mixture quantile function

## Examples

``` r
w <- c(0.50, 0.30, 0.20)
location <- c(-1, 0.5, 2.0)
scale <- c(1.0, 0.7, 1.4)

dLaplaceMix(0.8, w = w, location = location, scale = scale, log = FALSE)
#> [1] 0.2112312
pLaplaceMix(0.8, w = w, location = location, scale = scale,
           lower.tail = TRUE, log.p = FALSE)
#> [1] 0.7033967
qLaplaceMix(0.50, w = w, location = location, scale = scale)
#> [1] -0.0254679
qLaplaceMix(0.95, w = w, location = location, scale = scale)
#> [1] 3.183436
replicate(10, rLaplaceMix(1, w = w, location = location, scale = scale))
#>  [1] -1.6884628 -1.0172497  0.7002966  1.0742783 -1.1724444  2.4918533
#>  [7] -1.9789015 -1.7040411  0.5314815 -0.5180490
```
