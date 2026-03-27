# Laplace mixture with a GPD tail

Spliced bulk-tail family formed by attaching a generalized Pareto tail
to a Laplace mixture bulk.

## Usage

``` r
dLaplaceMixGpd(
  x,
  w,
  location,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  log = 0
)

pLaplaceMixGpd(
  q,
  w,
  location,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rLaplaceMixGpd(n, w, location, scale, threshold, tail_scale, tail_shape)

qLaplaceMixGpd(
  p,
  w,
  location,
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

- w:

  Numeric vector of mixture weights of length \\K\\.

- location:

  Numeric vector of length \\K\\ giving component locations.

- scale:

  Numeric vector of length \\K\\ giving component scales.

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

- tol:

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

- maxiter:

  Integer maximum iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qLaplaceMixGpd()` returns a numeric vector with the same length as `p`.

## Functions

- `dLaplaceMixGpd()`: Laplace mixture + GPD tail density

- `pLaplaceMixGpd()`: Laplace mixture + GPD tail distribution function

- `rLaplaceMixGpd()`: Laplace mixture + GPD tail random generation

- `qLaplaceMixGpd()`: Laplace mixture + GPD tail quantile function

## See also

[`laplace_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_mix.md),
[`laplace_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_gpd.md),
[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`laplace_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_lowercase.md),
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

Other laplace kernel families:
[`laplace_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_gpd.md),
[`laplace_mix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/laplace_mix.md)

## Examples

``` r
w <- c(0.50, 0.30, 0.20)
location <- c(-1, 0.5, 2.0)
scale <- c(1.0, 0.7, 1.4)
threshold <- 1
tail_scale <- 1.0
tail_shape <- 0.2

dLaplaceMixGpd(2.0, w = w, location = location, scale = scale,
              threshold = threshold, tail_scale = tail_scale,
              tail_shape = tail_shape, log = FALSE)
#> [1] 0.0865078
pLaplaceMixGpd(2.0, w = w, location = location, scale = scale,
              threshold = threshold, tail_scale = tail_scale,
              tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.8961906
qLaplaceMixGpd(0.50, w = w, location = location, scale = scale,
              threshold = threshold, tail_scale = tail_scale,
              tail_shape = tail_shape)
#> [1] -0.02546085
qLaplaceMixGpd(0.95, w = w, location = location, scale = scale,
              threshold = threshold, tail_scale = tail_scale,
              tail_shape = tail_shape)
#> [1] 2.943917
replicate(10, rLaplaceMixGpd(1, w = w, location = location, scale = scale,
                            threshold = threshold,
                            tail_scale = tail_scale,
                            tail_shape = tail_shape))
#>  [1]  0.5347682  2.2306633  1.4405908  2.5020907  6.5262022  6.1675301
#>  [7] -0.5760894 -0.6940654  0.6934115 -1.1575630
```
