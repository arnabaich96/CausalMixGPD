# Lowercase vectorized normal distribution functions

Vectorized R wrappers for the scalar normal-kernel topics in this file.
These helpers are meant for interactive use and examples rather than
direct use inside NIMBLE code.

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

## Details

These wrappers vectorize the scalar normal-kernel routines for ordinary
R use. They preserve the same formulas, parameter meanings, and tail
construction as the uppercase functions; the only change is that `x`,
`q`, `p`, and `n` may now be length greater than one.

For the mixture quantile and splice quantile functions, the numerical
and piecewise logic is delegated directly to the corresponding scalar
routine. As a result, the lowercase helpers are faithful front ends
rather than separate implementations.

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

## See also

[`normal_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_mix.md),
[`normal_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_mixgpd.md),
[`normal_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_gpd.md),
[`bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`get_kernel_registry()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/get_kernel_registry.md).

Other vectorized kernel helpers:
[`amoroso_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_lowercase.md),
[`base_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md),
[`cauchy_mix_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy_mix_lowercase.md),
[`gamma_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md),
[`invgauss_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/invgauss_lowercase.md),
[`laplace_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md),
[`lognormal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_lowercase.md)

## Examples

``` r
w <- c(0.6, 0.25, 0.15)
mu <- c(-1, 0.5, 2)
sig <- c(1, 0.7, 1.3)

# Normal mixture
dnormmix(c(0, 1, 2), w = w, mean = mu, sd = sig)
#> [1] 0.26967693 0.17703568 0.06303415
rnormmix(5, w = w, mean = mu, sd = sig)
#> [1]  3.9212585 -1.3780286  1.5338650 -0.1731000  0.2224225

# Normal mixture + GPD
dnormmixgpd(c(1, 2, 3), w = w, mean = mu, sd = sig,
            threshold = 2, tail_scale = 1, tail_shape = 0.2)
#> [1] 0.17703568 0.07982551 0.02673340

# Normal + GPD (single component)
dnormgpd(c(1, 2, 3), mean = 0.5, sd = 1, threshold = 2,
         tail_scale = 1, tail_shape = 0.2)
#> [1] 0.3520653 0.0668072 0.0223736
```
