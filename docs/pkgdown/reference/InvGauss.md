# Inverse Gaussian (Wald) distribution

The inverse Gaussian (also called the Wald distribution) is a
positive-support model that is right-skewed and often used for waiting
times. This package provides NIMBLE-compatible density, CDF, and RNG
functions under the `mean`/`shape` parameterization (mean \\\mu\>0\\,
shape \\\lambda\>0\\). A standalone mixture quantile function is
computed by numerical inversion elsewhere; `qInvGauss` inverts the base
CDF. These uppercase NIMBLE-compatible functions are scalar (`x`/`q` and
`n = 1`). For vectorized R usage (including `n > 1`), use
[`base_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md).

## Usage

``` r
dInvGauss(x, mean, shape, log = 0)

pInvGauss(q, mean, shape, lower.tail = 1, log.p = 0)

rInvGauss(n, mean, shape)

qInvGauss(
  p,
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

- mean:

  Numeric scalar mean parameter \\\mu\>0\\.

- shape:

  Numeric scalar shape parameter \\\lambda\>0\\.

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

  Numeric scalar giving the probability for the quantile.

- tol:

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

`dInvGauss` returns a numeric scalar density; `pInvGauss` returns a
numeric scalar CDF; `rInvGauss` returns one random draw; `qInvGauss`
returns a numeric quantile.

## Functions

- `dInvGauss()`: Inverse Gaussian density function

- `pInvGauss()`: Inverse Gaussian distribution function

- `rInvGauss()`: Inverse Gaussian random generation

- `qInvGauss()`: Inverse Gaussian quantile function

## Examples

``` r
mean <- 2
shape <- 5

dInvGauss(2.0, mean, shape, log = 0)
#> [1] 0.3153916
pInvGauss(2.0, mean, shape, lower.tail = 1, log.p = 0)
#> [1] 0.6161631
qInvGauss(0.50, mean, shape)
#> [1] 1.673117
qInvGauss(0.95, mean, shape)
#> [1] 4.458125
replicate(10, rInvGauss(1, mean, shape))
#>  [1] 1.3532062 0.9911095 2.6514150 1.4436713 2.3317355 4.3780345 1.7225800
#>  [8] 1.0503814 4.2435019 2.4257458
```
