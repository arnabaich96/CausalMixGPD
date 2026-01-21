# Normal mixture distribution

A finite mixture of Normal components. Base Normal functions are taken
from stats. Mixture density and CDF are computed by weighted sums.
Random generation samples a component index according to weights and
draws from the corresponding component. Quantiles are computed by
numerical inversion of the mixture CDF. The `d*`, `p*`, and `q*`
functions accept vector inputs for their first argument and evaluate
elementwise; `r*` supports `n > 1`.

## Usage

``` r
dNormMix(x, w, mean, sd, log = 0)

pNormMix(q, w, mean, sd, lower.tail = 1, log.p = 0)

rNormMix(n, w, mean, sd)

qNormMix(
  p,
  w,
  mean,
  sd,
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

- mean, sd:

  Numeric vectors of length \\K\\ giving component means and standard
  deviations.

- log:

  Integer flag `0/1`; if `1`, return the log-density.

- q:

  Numeric scalar giving the point at which the distribution function is
  evaluated.

- lower.tail:

  Integer flag `0/1`; if `1` (default), probabilities are \\P(X \le
  q)\\.

- log.p:

  Integer flag `0/1`; if `1`, probabilities are returned on the log
  scale.

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

Density/CDF/RNG functions return numeric scalars. `qNormMix` returns a
numeric vector with the same length as `p`.

## Functions

- `dNormMix()`: Normal mixture density

- `pNormMix()`: Normal mixture distribution function

- `rNormMix()`: Normal mixture random generation

- `qNormMix()`: Normal mixture quantile function

## Examples

``` r
w <- c(0.60, 0.25, 0.15)
mean <- c(-1, 0.5, 2.0)
sd <- c(1.0, 0.7, 1.3)

dNormMix(0.5, w = w, mean = mean, sd = sd, log = FALSE)
#> [1] 0.2438468
pNormMix(0.5, w = w, mean = mean, sd = sd,
        lower.tail = TRUE, log.p = FALSE)
#> [1] 0.7035579
qNormMix(0.50, w = w, mean = mean, sd = sd)
#> [1] -0.2713211
qNormMix(0.95, w = w, mean = mean, sd = sd)
#> [1] 2.571684
replicate(10, rNormMix(1, w = w, mean = mean, sd = sd))
#>  [1] -1.7006008  1.6953608  1.5137461 -2.3928471 -0.3570476  0.4439485
#>  [7] -0.6716351  3.0224017 -0.4555139  0.8685206
```
