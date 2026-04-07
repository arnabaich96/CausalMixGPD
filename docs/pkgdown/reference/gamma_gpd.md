# Gamma with a GPD tail

Spliced family obtained by attaching a generalized Pareto tail above
`threshold` to a single gamma bulk distribution.

## Usage

``` r
dGammaGpd(x, shape, scale, threshold, tail_scale, tail_shape, log = 0)

pGammaGpd(
  q,
  shape,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rGammaGpd(n, shape, scale, threshold, tail_scale, tail_shape)

qGammaGpd(
  p,
  shape,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)
```

## Arguments

- x:

  Numeric scalar giving the point at which the density is evaluated.

- shape:

  Numeric scalar Gamma shape parameter.

- scale:

  Numeric scalar scale parameter for the Gamma bulk.

- threshold:

  Numeric scalar threshold at which the GPD tail is attached.

- tail_scale:

  Numeric scalar GPD scale parameter; must be positive.

- tail_shape:

  Numeric scalar GPD shape parameter.

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

  Numeric tolerance for numerical inversion in `qGammaGpd`.

- maxiter:

  Maximum iterations for numerical inversion in `qGammaGpd`.

## Value

Spliced density/CDF/RNG functions return numeric scalars. `qGammaGpd()`
returns a numeric vector with the same length as `p`.

## Details

This topic combines a single gamma bulk with a generalized Pareto
exceedance model. If \\F\_\Gamma(u)\\ is the bulk probability below the
threshold, then the splice replaces the upper tail by
\\\\1-F\_\Gamma(u)\\g\_{GPD}(x)\\ while leaving the lower region
unchanged. The resulting distribution is continuous at the threshold and
preserves the gamma body exactly below \\u\\.

The ordinary mean is finite only when the GPD shape satisfies \\\xi \<
1\\. For heavier tails, predictive mean summaries should be replaced by
restricted means or quantile summaries.

## Functions

- `dGammaGpd()`: Gamma + GPD tail density

- `pGammaGpd()`: Gamma + GPD tail distribution function

- `rGammaGpd()`: Gamma + GPD tail random generation

- `qGammaGpd()`: Gamma + GPD tail quantile function

## See also

[`gamma_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mix.md),
[`gamma_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mixgpd.md),
[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`gamma_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md).

Other gamma kernel families:
[`gamma_mix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mix.md),
[`gamma_mixgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mixgpd.md)

## Examples

``` r
scale <- 2.5
shape <- 4
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dGammaGpd(4.0, scale = scale, shape = shape,
         threshold = threshold, tail_scale = tail_scale,
         tail_shape = tail_shape, log = 0)
#> [1] 0.3220605
pGammaGpd(4.0, scale = scale, shape = shape,
         threshold = threshold, tail_scale = tail_scale,
         tail_shape = tail_shape, lower.tail = 1, log.p = 0)
#> [1] 0.6457335
qGammaGpd(0.50, scale = scale, shape = shape,
         threshold = threshold, tail_scale = tail_scale,
         tail_shape = tail_shape)
#> [1] 3.63375
qGammaGpd(0.95, scale = scale, shape = shape,
         threshold = threshold, tail_scale = tail_scale,
         tail_shape = tail_shape)
#> [1] 6.636445
replicate(10, rGammaGpd(1, scale = scale, shape = shape,
                       threshold = threshold,
                       tail_scale = tail_scale,
                       tail_shape = tail_shape))
#>  [1] 6.607379 3.029453 5.115621 3.217602 6.531285 4.597788 5.324637 8.729386
#>  [9] 4.482273 4.500628
```
