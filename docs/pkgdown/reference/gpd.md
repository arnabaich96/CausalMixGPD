# Generalized Pareto distribution

Scalar generalized Pareto distribution (GPD) utilities for threshold
exceedances above `threshold`. These NIMBLE-compatible functions provide
the tail component used by the spliced bulk-tail families elsewhere in
the package.

## Usage

``` r
dGpd(x, threshold, scale, shape, log = 0)

pGpd(q, threshold, scale, shape, lower.tail = 1, log.p = 0)

rGpd(n, threshold, scale, shape)

qGpd(p, threshold, scale, shape, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- x:

  Numeric scalar giving the point at which the density is evaluated.

- threshold:

  Numeric scalar threshold at which the GPD is attached.

- scale:

  Numeric scalar GPD scale parameter; must be positive.

- shape:

  Numeric scalar GPD shape parameter.

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

## Value

`dGpd()` returns a numeric scalar density, `pGpd()` returns a numeric
scalar CDF, `rGpd()` returns one random draw, and `qGpd()` returns a
numeric quantile.

## Details

The parameterization is \$\$ G(x) = 1 - \left(1 + \xi \frac{x -
u}{\sigma_u}\right)^{-1 / \xi}, \qquad x \ge u, \$\$ where
`threshold = u`, `scale = sigma_u > 0`, and `shape = xi`. When `shape`
approaches zero, the distribution reduces to the exponential tail limit.

These uppercase NIMBLE-compatible functions are scalar (`x`/`q` and
`n = 1`). For vectorized R usage, use
[`base_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md).

If a bulk distribution has CDF \\F\_{bulk}\\, the package's spliced
families typically use the tail construction \$\$ F(x) = \left\\
\begin{array}{ll} F\_{bulk}(x), & x \< u, \\ F\_{bulk}(u) + \\1 -
F\_{bulk}(u)\\ G(x), & x \ge u. \end{array} \right. \$\$

## Functions

- `dGpd()`: Generalized Pareto density function

- `pGpd()`: Generalized Pareto distribution function

- `rGpd()`: Generalized Pareto random generation

- `qGpd()`: Generalized Pareto quantile function

## See also

[`base_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md),
[`normal_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_gpd.md),
[`lognormal_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_gpd.md),
[`gamma_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_gpd.md),
[`InvGauss_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss_gpd.md),
[`laplace_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_gpd.md),
[`amoroso_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_gpd.md).

## Examples

``` r
threshold <- 1
tail_scale <- 0.8
tail_shape <- 0.2

dGpd(1.5, threshold, tail_scale, tail_shape, log = 0)
#> [1] 0.6165877
pGpd(1.5, threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
#> [1] 0.445071
qGpd(0.50, threshold, tail_scale, tail_shape)
#> [1] 1.594793
qGpd(0.95, threshold, tail_scale, tail_shape)
#> [1] 4.282257
replicate(10, rGpd(1, threshold, tail_scale, tail_shape))
#>  [1] 2.282323 2.156120 1.623547 5.354201 1.374512 1.152272 2.430307 1.025209
#>  [9] 2.462315 1.148486
```
