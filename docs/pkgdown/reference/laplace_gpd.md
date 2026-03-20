# Laplace with a GPD tail

Spliced family obtained by attaching a generalized Pareto tail above
`threshold` to a single Laplace bulk.

## Usage

``` r
dLaplaceGpd(x, location, scale, threshold, tail_scale, tail_shape, log = 0)

pLaplaceGpd(
  q,
  location,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rLaplaceGpd(n, location, scale, threshold, tail_scale, tail_shape)

qLaplaceGpd(
  p,
  location,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)
```

## Arguments

- x:

  Numeric scalar giving the point at which the density is evaluated.

- location:

  Numeric scalar location parameter for the Laplace bulk.

- scale:

  Numeric scalar scale parameter for the Laplace bulk.

- threshold:

  Numeric scalar threshold at which the GPD tail is attached.

- tail_scale:

  Numeric scalar GPD scale parameter; must be positive.

- tail_shape:

  Numeric scalar GPD shape parameter.

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

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qLaplaceGpd()` returns a numeric vector with the same length as `p`.

## Functions

- `dLaplaceGpd()`: Laplace + GPD tail density

- `pLaplaceGpd()`: Laplace + GPD tail distribution function

- `rLaplaceGpd()`: Laplace + GPD tail random generation

- `qLaplaceGpd()`: Laplace + GPD tail quantile function

## See also

[`laplace_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_mix.md),
[`laplace_MixGpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_MixGpd.md),
[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`laplace_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md).

Other laplace kernel families:
[`laplace_MixGpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_MixGpd.md),
[`laplace_mix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_mix.md)

## Examples

``` r
location <- 0.5
scale <- 1.0
threshold <- 1
tail_scale <- 1.0
tail_shape <- 0.2

dLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape, log = FALSE)
#> [1] 0.1015629
pLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape,
           lower.tail = TRUE, log.p = FALSE)
#> [1] 0.8781245
qLaplaceGpd(0.50, location, scale, threshold, tail_scale, tail_shape)
#> [1] 0.5
qLaplaceGpd(0.95, location, scale, threshold, tail_scale, tail_shape)
#> [1] 3.170353
replicate(10, rLaplaceGpd(1, location, scale, threshold, tail_scale, tail_shape))
#>  [1]  0.64116940 -0.45552592  0.19260121  1.02705848  2.79357033 -0.24057496
#>  [7] -1.34531772  0.04364005  1.82800703 -0.60544443
```
