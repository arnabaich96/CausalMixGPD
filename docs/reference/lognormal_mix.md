# Lognormal mixture distribution

A finite mixture of Lognormal components. Base Lognormal functions are
taken from stats. Mixture density and CDF are computed by weighted sums.
Random generation samples a component index according to weights and
draws from the corresponding component. Quantiles are computed by
numerical inversion of the mixture CDF. The `d*`, `p*`, and `q*`
functions accept vector inputs for their first argument and evaluate
elementwise; `r*` supports `n > 1`.

## Usage

``` r
dLognormalMix(x, w, meanlog, sdlog, log = 0)

pLognormalMix(q, w, meanlog, sdlog, lower.tail = 1, log.p = 0)

rLognormalMix(n, w, meanlog, sdlog)

qLognormalMix(
  p,
  w,
  meanlog,
  sdlog,
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

- meanlog, sdlog:

  Numeric vectors of length \\K\\ giving component log-means and
  log-standard deviations.

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

Density/CDF/RNG functions return numeric scalars. `qLognormalMix`
returns a numeric vector with the same length as `p`.

## Functions

- `dLognormalMix()`: Lognormal mixture density

- `pLognormalMix()`: Lognormal mixture distribution function

- `rLognormalMix()`: Lognormal mixture random generation

- `qLognormalMix()`: Lognormal mixture quantile function

## Examples

``` r
w <- c(0.60, 0.25, 0.15)
meanlog <- c(-0.2, 0.6, 1.2)
sdlog <- c(0.4, 0.3, 0.5)

dLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog, log = FALSE)
#> [1] 0.2189383
pLognormalMix(2.0, w = w, meanlog = meanlog, sdlog = sdlog,
             lower.tail = TRUE, log.p = FALSE)
#> [1] 0.7711135
qLognormalMix(0.50, w = w, meanlog = meanlog, sdlog = sdlog)
#> [1] 1.151134
qLognormalMix(0.95, w = w, meanlog = meanlog, sdlog = sdlog)
#> [1] 4.147585
replicate(10, rLognormalMix(1, w = w, meanlog = meanlog, sdlog = sdlog))
#>  [1] 5.2456834 2.3325388 0.8745521 1.8161562 0.5021116 1.3068884 0.4199004
#>  [8] 5.4610490 0.6691172 0.7624799
```
