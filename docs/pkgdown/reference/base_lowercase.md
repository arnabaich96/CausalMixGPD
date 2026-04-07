# Lowercase vectorized distribution functions (base kernels)

Vectorized R wrappers around the scalar base-kernel functions defined in
this file. These helpers are intended for interactive R use, examples,
testing, and checking numerical behavior outside compiled NIMBLE code.

## Usage

``` r
dgpd(x, threshold, scale, shape, log = FALSE)

pgpd(q, threshold, scale, shape, lower.tail = TRUE, log.p = FALSE)

qgpd(p, threshold, scale, shape, lower.tail = TRUE, log.p = FALSE)

rgpd(n, threshold, scale, shape)

dinvgauss(x, mean, shape, log = FALSE)

pinvgauss(q, mean, shape, lower.tail = TRUE, log.p = FALSE)

qinvgauss(
  p,
  mean,
  shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rinvgauss(n, mean, shape)

damoroso(x, loc, scale, shape1, shape2, log = FALSE)

pamoroso(q, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE)

qamoroso(p, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE)

ramoroso(n, loc, scale, shape1, shape2)

dcauchy_vec(x, location, scale, log = FALSE)

pcauchy_vec(q, location, scale, lower.tail = TRUE, log.p = FALSE)

qcauchy_vec(p, location, scale, lower.tail = TRUE, log.p = FALSE)

rcauchy_vec(n, location, scale)
```

## Arguments

- x:

  Numeric vector of quantiles.

- threshold, scale, shape, mean, loc, shape1, shape2, location:

  Distribution parameters (scalars).

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

- n:

  Integer number of observations to generate.

- tol, maxiter:

  Tolerance and max iterations for numerical inversion.

## Value

Numeric vector of densities, probabilities, quantiles, or random
variates.

## Details

The wrappers preserve the same parameterizations as the uppercase scalar
functions, but accept vector inputs for `x`, `q`, or `p` and allow
`n > 1` for random generation.

Each lowercase helper is a vectorized R wrapper around the corresponding
uppercase scalar routine documented in this file. The wrapper keeps the
same parameterization and simply applies the scalar kernel repeatedly
over the supplied evaluation points or simulation index. These helpers
are therefore appropriate for interactive analysis, testing, and
examples, whereas the uppercase functions are the building blocks used
inside NIMBLE model code.

The wrappers do not change the underlying theory. For example, `qgpd()`
still uses the closed-form GPD inverse, `qinvgauss()` still performs
numerical inversion of the inverse Gaussian CDF, and `qamoroso()` still
maps a gamma quantile through the Amoroso transformation.
Random-generation wrappers call the corresponding scalar RNG repeatedly
when `n > 1`.

## Functions

- `dgpd()`: GPD density (vectorized)

- `pgpd()`: GPD distribution function (vectorized)

- `qgpd()`: GPD quantile function (vectorized)

- `rgpd()`: GPD random generation (vectorized)

- `dinvgauss()`: Inverse Gaussian density (vectorized)

- `pinvgauss()`: Inverse Gaussian distribution function (vectorized)

- `qinvgauss()`: Inverse Gaussian quantile function (vectorized)

- `rinvgauss()`: Inverse Gaussian random generation (vectorized)

- `damoroso()`: Amoroso density (vectorized)

- `pamoroso()`: Amoroso distribution function (vectorized)

- `qamoroso()`: Amoroso quantile function (vectorized)

- `ramoroso()`: Amoroso random generation (vectorized)

- `dcauchy_vec()`: Cauchy density (vectorized)

- `pcauchy_vec()`: Cauchy distribution function (vectorized)

- `qcauchy_vec()`: Cauchy quantile function (vectorized)

- `rcauchy_vec()`: Cauchy random generation (vectorized)

## See also

[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`InvGauss()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss.md),
[`amoroso()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso.md),
[`cauchy()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy.md),
[`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`kernel_support_table()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md).

Other vectorized kernel helpers:
[`amoroso_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_lowercase.md),
[`cauchy_mix_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy_mix_lowercase.md),
[`gamma_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md),
[`invgauss_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/invgauss_lowercase.md),
[`laplace_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md),
[`lognormal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_lowercase.md),
[`normal_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_lowercase.md)

## Examples

``` r
# GPD
dgpd(c(1.5, 2.0, 2.5), threshold = 1, scale = 0.8, shape = 0.2)
#> [1] 0.6165877 0.3276800 0.1849668
pgpd(c(1.5, 2.0), threshold = 1, scale = 0.8, shape = 0.2)
#> [1] 0.445071 0.672320
qgpd(c(0.5, 0.9), threshold = 1, scale = 0.8, shape = 0.2)
#> [1] 1.594793 3.339573
rgpd(5, threshold = 1, scale = 0.8, shape = 0.2)
#> [1] 1.457890 1.989540 1.608985 1.964277 1.616777

# Inverse Gaussian
dinvgauss(c(1, 2, 3), mean = 2, shape = 5)
#> [1] 0.4774864 0.3153916 0.1393911
rinvgauss(5, mean = 2, shape = 5)
#> [1] 3.673805 7.611777 2.000607 2.288698 2.224759

# Amoroso
damoroso(c(1, 2), loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2)
#> [1] 0.2452362 0.2915082
ramoroso(5, loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2)
#> [1] 4.2921093 1.4464358 1.9674029 3.5421019 0.9699588

# Cauchy
dcauchy_vec(c(-1, 0, 1), location = 0, scale = 1)
#> [1] 0.1591549 0.3183099 0.1591549
rcauchy_vec(5, location = 0, scale = 1)
#> [1] -1.4257585 -1.7587124  0.5629465  2.0670801  4.2562905
```
