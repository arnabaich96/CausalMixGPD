# Gamma mixture distribution

A finite mixture of Gamma components. Base Gamma functions are taken
from stats. Mixture density and CDF are computed by weighted sums.
Random generation samples a component according to weights and draws
from the corresponding component. Quantiles are computed by numerical
inversion of the mixture CDF. These uppercase NIMBLE-compatible
functions are scalar (`x`/`q` and `n = 1`). For vectorized R usage
(including `n > 1`), use
[`gamma_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md).

## Usage

``` r
dGammaMix(x, w, shape, scale, log = 0)

pGammaMix(q, w, shape, scale, lower.tail = 1, log.p = 0)

rGammaMix(n, w, shape, scale)

qGammaMix(
  p,
  w,
  shape,
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

- shape, scale:

  Numeric vectors of length \\K\\ giving Gamma shapes and rates.

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

Density/CDF/RNG functions return numeric scalars. `qGammaMix` returns a
numeric vector with the same length as `p`.

## Functions

- `dGammaMix()`: Gamma mixture density

- `pGammaMix()`: Gamma mixture distribution function

- `rGammaMix()`: Gamma mixture random generation

- `qGammaMix()`: Gamma mixture quantile function

## Examples

``` r
w <- c(0.55, 0.30, 0.15)
scale <- c(1.0, 2.5, 5.0)
shape <- c(2, 4, 6)

dGammaMix(2.0, w = w, scale = scale, shape = shape, log = 0)
#> [1] 0.1534717
pGammaMix(2.0, w = w, scale = scale, shape = shape, lower.tail = 1, log.p = 0)
#> [1] 0.3294213
qGammaMix(0.50, w = w, scale = scale, shape = shape)
#> [1] 3.623739
qGammaMix(0.95, w = w, scale = scale, shape = shape)
#> [1] 33.81667
replicate(10, rGammaMix(1, w = w, scale = scale, shape = shape))
#>  [1]  7.2810617 12.2948276  1.5274296 10.4751979 21.6543832  0.4246413
#>  [7]  9.1363824  1.1727450 17.8997664 24.2387414
```
