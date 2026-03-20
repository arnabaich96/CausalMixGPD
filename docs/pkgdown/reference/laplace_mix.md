# Laplace (double exponential) mixture distribution

Finite mixture of Laplace components for real-valued bulk modeling. The
scalar functions in this topic are the NIMBLE-compatible building blocks
for Laplace-based kernels.

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

Density/CDF/RNG functions return numeric scalars. `qLaplaceMix()`
returns a numeric vector with the same length as `p`.

## Details

The mixture density is \$\$ f(x) = \sum\_{k = 1}^K \tilde{w}\_k
f\_{Lap}(x \mid \mu_k, b_k), \$\$ with normalized weights
\\\tilde{w}\_k\\. For vectorized R usage, use
[`laplace_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md).

## Functions

- `dLaplaceMix()`: Laplace mixture density

- `pLaplaceMix()`: Laplace mixture distribution function

- `rLaplaceMix()`: Laplace mixture random generation

- `qLaplaceMix()`: Laplace mixture quantile function

## See also

[`laplace_MixGpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_MixGpd.md),
[`laplace_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_gpd.md),
[`laplace_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md),
[`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`kernel_support_table()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md).

Other laplace kernel families:
[`laplace_MixGpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_MixGpd.md),
[`laplace_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_gpd.md)

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
#> [1] -0.02546085
qLaplaceMix(0.95, w = w, location = location, scale = scale)
#> [1] 3.18341
replicate(10, rLaplaceMix(1, w = w, location = location, scale = scale))
#>  [1] -0.5459962  7.6818187  0.7002966  1.0742783 -1.1724444  2.4918533
#>  [7] -1.9789015 -1.7040411  0.5314815 -0.5180490
```
