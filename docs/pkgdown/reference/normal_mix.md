# Normal mixture distribution

Finite mixture of normal components for real-valued bulk modeling. This
topic provides the scalar mixture density, CDF, RNG, and quantile
functions used by the bulk-only normal kernel in the package registry.

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

Density/CDF/RNG functions return numeric scalars. `qNormMix()` returns a
numeric vector with the same length as `p`.

## Details

With weights \\w_k\\, means \\\mu_k\\, and standard deviations
\\\sigma_k\\, the mixture density is \$\$ f(x) = \sum\_{k = 1}^K
\tilde{w}\_k \phi(x \mid \mu_k, \sigma_k^2), \$\$ where \\\tilde{w}\_k =
w_k / \sum_j w_j\\. These uppercase functions are scalar
NIMBLE-compatible building blocks. For vectorized R usage, use
[`normal_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_lowercase.md).

If \\F_k\\ denotes the \\k\\-th component CDF, then the mixture
distribution function is \$\$ F(x) = \sum\_{k=1}^K \tilde{w}\_k F_k(x) =
\sum\_{k=1}^K \tilde{w}\_k \Phi\left(\frac{x-\mu_k}{\sigma_k}\right).
\$\$ Random generation first draws a component index with probability
\\\tilde{w}\_k\\ and then generates from the corresponding normal law.
The quantile function has no closed form for a general finite mixture,
so `qNormMix()` solves \\F(x)=p\\ numerically by bracketing the root and
applying [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

The mixture mean is \$\$ E(X) = \sum\_{k=1}^K \tilde{w}\_k \mu_k, \$\$
which is the analytical mean used by the package when a normal-mixture
draw contributes to a posterior predictive mean calculation.

## Functions

- `dNormMix()`: Normal mixture density

- `pNormMix()`: Normal mixture distribution function

- `rNormMix()`: Normal mixture random generation

- `qNormMix()`: Normal mixture quantile function

## See also

[`normal_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_mixgpd.md),
[`normal_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_gpd.md),
[`normal_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_lowercase.md),
[`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`kernel_support_table()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md).

Other normal kernel families:
[`normal_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_gpd.md),
[`normal_mixgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_mixgpd.md)

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
#>  [1] -1.6695334  0.4361516 -0.3634372  0.8686264 -0.1064025  0.1765331
#>  [7]  1.7688322  0.7074084 -3.9389776  1.0754396
```
