# Inverse Gaussian mixture distribution

A finite mixture of inverse Gaussian components provides a flexible bulk
model for positive data with right skewness. Each component is
parameterized by its mean `mean[j]` and shape `shape[j]`.

## Usage

``` r
dInvGaussMix(x, w, mean, shape, log = 0)

pInvGaussMix(q, w, mean, shape, lower.tail = 1, log.p = 0)

rInvGaussMix(n, w, mean, shape)

qInvGaussMix(
  p,
  w,
  mean,
  shape,
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

- mean, shape:

  Numeric vectors of length \\K\\ giving component means and shapes.

- log:

  Logical; if `TRUE`, return the log-density (integer flag `0/1` in
  NIMBLE).

- q:

  Numeric scalar giving the point at which the distribution function is
  evaluated.

- lower.tail:

  Logical; if `TRUE` (default), probabilities are \\P(X \le q)\\.

- log.p:

  Logical; if `TRUE`, probabilities are returned on the log scale.

- n:

  Integer giving the number of draws. For portability inside NIMBLE, the
  RNG implementation supports `n = 1`.

- p:

  Numeric scalar probability in \\(0,1)\\ for the quantile function.

- tol:

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html) in quantile
  inversion.

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Density/CDF/RNG functions return numeric scalars. `qInvGaussMix` returns
a numeric vector with the same length as `p`.

## Details

The density, CDF, and RNG are implemented as `nimbleFunction`s for use
inside NIMBLE models. The quantile function is an R function computed by
numerical inversion of the mixture CDF.

## Functions

- `dInvGaussMix()`: Inverse Gaussian mixture density

- `pInvGaussMix()`: Inverse Gaussian mixture distribution function

- `rInvGaussMix()`: Inverse Gaussian mixture random generation

- `qInvGaussMix()`: Inverse Gaussian mixture quantile function

## Examples

``` r
w <- c(0.55, 0.30, 0.15)
mean <- c(1.0, 2.5, 5.0)
shape <- c(2, 4, 8)

dInvGaussMix(2.0, w = w, mean = mean, shape = shape, log = 0)
#> [1] 0.17698
pInvGaussMix(2.0, w = w, mean = mean, shape = shape,
            lower.tail = 1, log.p = 0)
#> [1] 0.6866789
qInvGaussMix(0.50, w = w, mean = mean, shape = shape)
#> [1] 1.251694
qInvGaussMix(0.95, w = w, mean = mean, shape = shape)
#> [1] 6.489781
replicate(10, rInvGaussMix(1, w = w, mean = mean, shape = shape))
#>  [1] 1.1107784 0.6394550 2.6216670 4.6625934 1.2470273 1.0056430 0.2549927
#>  [8] 0.3752957 5.3715940 1.8885472
```
