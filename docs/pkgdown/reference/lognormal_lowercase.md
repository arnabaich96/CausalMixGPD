# Lowercase vectorized lognormal distribution functions

Vectorized R wrappers for the scalar lognormal-kernel topics in this
file.

## Usage

``` r
dlognormalmix(x, w, meanlog, sdlog, log = FALSE)

plognormalmix(q, w, meanlog, sdlog, lower.tail = TRUE, log.p = FALSE)

qlognormalmix(
  p,
  w,
  meanlog,
  sdlog,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rlognormalmix(n, w, meanlog, sdlog)

dlognormalmixgpd(
  x,
  w,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  log = FALSE
)

plognormalmixgpd(
  q,
  w,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qlognormalmixgpd(
  p,
  w,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rlognormalmixgpd(n, w, meanlog, sdlog, threshold, tail_scale, tail_shape)

dlognormalgpd(
  x,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  log = FALSE
)

plognormalgpd(
  q,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

qlognormalgpd(
  p,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)

rlognormalgpd(n, meanlog, sdlog, threshold, tail_scale, tail_shape)
```

## Arguments

- x:

  Numeric vector of quantiles.

- w:

  Numeric vector of mixture weights.

- meanlog, sdlog:

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

## Functions

- `dlognormalmix()`: Lognormal mixture density (vectorized)

- `plognormalmix()`: Lognormal mixture distribution function
  (vectorized)

- `qlognormalmix()`: Lognormal mixture quantile function (vectorized)

- `rlognormalmix()`: Lognormal mixture random generation (vectorized)

- `dlognormalmixgpd()`: Lognormal mixture + GPD density (vectorized)

- `plognormalmixgpd()`: Lognormal mixture + GPD distribution function
  (vectorized)

- `qlognormalmixgpd()`: Lognormal mixture + GPD quantile function
  (vectorized)

- `rlognormalmixgpd()`: Lognormal mixture + GPD random generation
  (vectorized)

- `dlognormalgpd()`: Lognormal + GPD density (vectorized)

- `plognormalgpd()`: Lognormal + GPD distribution function (vectorized)

- `qlognormalgpd()`: Lognormal + GPD quantile function (vectorized)

- `rlognormalgpd()`: Lognormal + GPD random generation (vectorized)

## See also

[`lognormal_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mix.md),
[`lognormal_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mixgpd.md),
[`lognormal_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_gpd.md),
[`bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/bundle.md),
[`get_kernel_registry()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/get_kernel_registry.md).

Other vectorized kernel helpers:
[`amoroso_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_lowercase.md),
[`base_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md),
[`cauchy_mix_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy_mix_lowercase.md),
[`gamma_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md),
[`invgauss_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/invgauss_lowercase.md),
[`laplace_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md),
[`normal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_lowercase.md)

## Examples

``` r
w <- c(0.6, 0.3, 0.1)
ml <- c(0, 0.3, 0.6)
sl <- c(0.4, 0.5, 0.6)

# Lognormal mixture
dlognormalmix(c(1, 2, 3), w = w, meanlog = ml, sdlog = sl)
#> [1] 0.8386766 0.1873719 0.0425651
rlognormalmix(5, w = w, meanlog = ml, sdlog = sl)
#> [1] 0.6360860 0.7556021 1.5831195 1.4558284 0.5728457

# Lognormal mixture + GPD
dlognormalmixgpd(c(2, 3, 4), w = w, meanlog = ml, sdlog = sl,
                 threshold = 2.5, tail_scale = 0.5, tail_shape = 0.2)
#> [1] 0.187371921 0.046321740 0.008244275
```
