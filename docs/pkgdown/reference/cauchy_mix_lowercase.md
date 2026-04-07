# Lowercase vectorized Cauchy mixture distribution functions

Vectorized R wrappers for the scalar Cauchy mixture functions in this
file.

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

## Details

These are vectorized R wrappers around the scalar Cauchy-mixture
routines. They retain the same location-scale parameterization and the
same inverse-CDF logic for simulation and quantiles. The lowercase
functions do not alter the heavy-tail theory of the underlying Cauchy
components; they simply make the scalar routines convenient to use on
vectors in R.

## Functions

- `dcauchymix()`: Cauchy mixture density (vectorized)

- `pcauchymix()`: Cauchy mixture distribution function (vectorized)

- `qcauchymix()`: Cauchy mixture quantile function (vectorized)

- `rcauchymix()`: Cauchy mixture random generation (vectorized)

## See also

[`cauchy_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy_mix.md),
[`cauchy()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy.md),
[`bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`get_kernel_registry()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/get_kernel_registry.md).

Other vectorized kernel helpers:
[`amoroso_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_lowercase.md),
[`base_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md),
[`gamma_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md),
[`invgauss_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/invgauss_lowercase.md),
[`laplace_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md),
[`lognormal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_lowercase.md),
[`normal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_lowercase.md)

## Examples

``` r
w <- c(0.6, 0.3, 0.1)
loc <- c(-1, 0, 1)
scl <- c(1, 1.2, 2)

dcauchymix(c(-2, 0, 2), w = w, location = loc, scale = scl)
#> [1] 0.12145467 0.18780283 0.05289561
rcauchymix(5, w = w, location = loc, scale = scl)
#> [1] -0.35693348 -0.07215709 -4.37432663 -4.86462883 -3.45710687
```
