# Gamma mixture distribution

Finite mixture of gamma components for positive-support bulk modeling.
The scalar functions in this topic are the compiled building blocks
behind the gamma bulk kernel family.

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

Density/CDF/RNG functions return numeric scalars. `qGammaMix()` returns
a numeric vector with the same length as `p`.

## Details

The mixture density is \$\$ f(x) = \sum\_{k = 1}^K \tilde{w}\_k
f\_{\Gamma}(x \mid \alpha_k, \theta_k), \qquad x \> 0, \$\$ with
normalized weights \\\tilde{w}\_k\\. For vectorized R usage, use
[`gamma_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md).

## Functions

- `dGammaMix()`: Gamma mixture density

- `pGammaMix()`: Gamma mixture distribution function

- `rGammaMix()`: Gamma mixture random generation

- `qGammaMix()`: Gamma mixture quantile function

## See also

[`gamma_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mixgpd.md),
[`gamma_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_gpd.md),
[`gamma_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md),
[`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`kernel_support_table()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md).

Other gamma kernel families:
[`gamma_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_gpd.md),
[`gamma_mixgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mixgpd.md)

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
#>  [1]  7.4874491  7.6825718 58.5380719  0.7576791 10.4751979 21.6543832
#>  [7]  0.4246413  9.1363824  1.1727450 17.8997664
```
