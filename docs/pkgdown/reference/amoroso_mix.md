# Amoroso mixture distribution

Finite mixture of Amoroso components for flexible positive-support bulk
modeling.

## Usage

``` r
dAmorosoMix(x, w, loc, scale, shape1, shape2, log = 0)

pAmorosoMix(q, w, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)

rAmorosoMix(n, w, loc, scale, shape1, shape2)

qAmorosoMix(
  p,
  w,
  loc,
  scale,
  shape1,
  shape2,
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

  Numeric vector of mixture weights of length \\K\\. The functions treat
  the weights as non-negative and normalize them internally when needed.

- loc:

  Numeric vector of length \\K\\ giving component locations.

- scale:

  Numeric vector of length \\K\\ giving component scales. Negative
  values flip support for the corresponding component.

- shape1:

  Numeric vector of length \\K\\ giving the first Amoroso shape
  parameter for each component.

- shape2:

  Numeric vector of length \\K\\ giving the second Amoroso shape
  parameter for each component.

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

Mixture density/CDF/RNG functions return numeric scalars.
`qAmorosoMix()` returns a numeric vector with the same length as `p`.

## Details

The mixture density is \$\$ f(x) = \sum\_{k = 1}^K \tilde{w}\_k
f\_{Amoroso}(x \mid a_k, \theta_k, \alpha_k, \beta_k), \$\$ with
normalized weights \\\tilde{w}\_k\\. These scalar functions are
NIMBLE-compatible; for vectorized R usage, use
[`amoroso_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_lowercase.md).

The Amoroso family is especially useful for positive-support data
because it can reproduce a wide range of skewed and heavy-right-tail
shapes while remaining analytically tractable through its gamma
transformation. The mixture CDF is \$\$ F(x) = \sum\_{k=1}^K
\tilde{w}\_k F\_{Amoroso}(x \mid a_k,\theta_k,\alpha_k,\beta_k), \$\$
and random generation proceeds by selecting a component and sampling
from that component.

Closed-form mixture quantiles are not available, so `qAmorosoMix()`
inverts the mixture CDF numerically. The analytical mixture mean is the
weighted average of the component means, \\a_k + \theta_k
\Gamma(\alpha_k + 1/\beta_k) / \Gamma(\alpha_k)\\, whenever those
component moments exist.

## Functions

- `dAmorosoMix()`: Density Function of Amoroso Mixture Distribution

- `pAmorosoMix()`: Cumulative Distribution Function of Amoroso Mixture
  Distribution

- `rAmorosoMix()`: Random Generation for Amoroso Mixture Distribution

- `qAmorosoMix()`: Quantile Function of Amoroso Mixture Distribution

## See also

[`amoroso_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_mixgpd.md),
[`amoroso_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_gpd.md),
[`amoroso_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_lowercase.md),
[`build_nimble_bundle()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/build_nimble_bundle.md),
[`kernel_support_table()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md).

Other amoroso kernel families:
[`amoroso_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_gpd.md),
[`amoroso_mixgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_mixgpd.md)

## Examples

``` r
w <- c(0.60, 0.25, 0.15)
loc <- c(0, 1, 2)
scale <- c(1.0, 1.2, 1.6)
shape1 <- c(2, 4, 6)
shape2 <- c(1.0, 1.2, 1.5)

dAmorosoMix(2.0, w, loc, scale, shape1, shape2, log = 0)
#> [1] 0.1717337
pAmorosoMix(2.0, w, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)
#> [1] 0.3587001
qAmorosoMix(0.50, w, loc, scale, shape1, shape2)
#> [1] 2.929829
qAmorosoMix(0.95, w, loc, scale, shape1, shape2)
#> [1] 8.017293
replicate(10, rAmorosoMix(1, w, loc, scale, shape1, shape2))
#>  [1] 6.6966960 1.1390523 1.7812916 2.8622365 2.5033891 1.2467692 3.6989854
#>  [8] 1.1954265 0.8008628 7.2399839
```
