# Inverse Gaussian mixture distribution

Finite mixture of inverse Gaussian components for positive-support bulk
modeling. Each component is parameterized by `mean[j]` and `shape[j]`.

## Usage

``` r
dInvGaussMix(x, w, mean, shape, log = 0)

pInvGaussMix(q, w, mean, shape, lower.tail = 1, log.p = 0)

rInvGaussMix(n, w, mean, shape)

qInvGaussMix(
  p,
  w,
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

- w:

  Numeric vector of mixture weights of length \\K\\. The functions
  normalize `w` internally when needed.

- mean, shape:

  Numeric vectors of length \\K\\ giving component means and shapes.

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

  Numeric scalar probability in \\(0,1)\\ for the quantile function.

- tol:

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html) in quantile
  inversion.

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Density/CDF/RNG functions return numeric scalars. `qInvGaussMix()`
returns a numeric vector with the same length as `p`.

## Details

The scalar functions in this topic are the compiled building blocks for
inverse-Gaussian bulk kernels. For vectorized R usage, use
[`invgauss_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/invgauss_lowercase.md).

The mixture distribution is \$\$ F(x) = \sum\_{k=1}^K \tilde{w}\_k
F\_{IG}(x \mid \mu_k,\lambda_k), \$\$ where each inverse Gaussian
component has mean \\\mu_k\\ and variance \\\mu_k^3/\lambda_k\\. Random
generation selects a component using the normalized weights and then
generates from the corresponding inverse Gaussian law. Quantiles are
computed numerically because the finite-mixture inverse CDF is not
available in closed form.

The analytical mixture mean is \$\$ E(X) = \sum\_{k=1}^K \tilde{w}\_k
\mu_k. \$\$ That expression is used by the package whenever
inverse-Gaussian mixtures contribute to posterior predictive means.

## Functions

- `dInvGaussMix()`: Inverse Gaussian mixture density

- `pInvGaussMix()`: Inverse Gaussian mixture distribution function

- `rInvGaussMix()`: Inverse Gaussian mixture random generation

- `qInvGaussMix()`: Inverse Gaussian mixture quantile function

## See also

[`InvGauss_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mixgpd.md),
[`InvGauss_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_gpd.md),
[`invgauss_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/invgauss_lowercase.md),
[`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`kernel_support_table()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md).

Other inverse-gaussian kernel families:
[`InvGauss_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_gpd.md),
[`InvGauss_mixgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_mixgpd.md)

## Examples

``` r
w <- c(0.55, 0.30, 0.15)
mean <- c(1.0, 2.5, 5.0)
shape <- c(2, 4, 8)

dInvGaussMix(2.0, w = w, mean = mean, shape = shape, log = 0)
#> [1] 0.17698
pInvGaussMix(2.0, w = w, mean = mean, shape = shape,
            lower.tail = 1, log.p = 0)
#> [1] 0.6866789
qInvGaussMix(0.50, w = w, mean = mean, shape = shape)
#> [1] 1.251694
qInvGaussMix(0.95, w = w, mean = mean, shape = shape)
#> [1] 6.489781
replicate(10, rInvGaussMix(1, w = w, mean = mean, shape = shape))
#>  [1] 2.0679382 0.5774593 3.2675875 1.4805399 7.4575032 1.9138819 1.0288358
#>  [8] 5.4002522 1.9116509 0.6089238
```
