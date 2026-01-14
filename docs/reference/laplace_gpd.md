# Laplace with a Gpd tail

Splices a generalized Pareto distribution (Gpd) above `threshold` onto a
single Laplace bulk with parameters `location` and `scale`. Base Laplace
functions are taken from nimble.

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

  Numeric scalar threshold at which the Gpd tail is attached.

- tail_scale:

  Numeric scalar Gpd scale parameter; must be positive.

- tail_shape:

  Numeric scalar Gpd shape parameter.

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

Spliced density/CDF/RNG functions return numeric scalars. `qLaplaceGpd`
returns a numeric vector with the same length as `p`.

## Functions

- `dLaplaceGpd()`: Laplace + Gpd tail density

- `pLaplaceGpd()`: Laplace + Gpd tail distribution function

- `rLaplaceGpd()`: Laplace + Gpd tail random generation

- `qLaplaceGpd()`: Laplace + Gpd tail quantile function

## Examples

``` r
location <- 0.5
scale <- 1.0
threshold <- 1
tail_scale <- 1.0
tail_shape <- 0.2

dLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape, log = FALSE)
#> [1] 3.032653e-301
pLaplaceGpd(2.0, location, scale, threshold, tail_scale, tail_shape,
           lower.tail = TRUE, log.p = FALSE)
#> [1] 0.8781245
qLaplaceGpd(0.50, location, scale, threshold, tail_scale, tail_shape)
#> [1] 0.5
qLaplaceGpd(0.95, location, scale, threshold, tail_scale, tail_shape)
#> [1] 3.170353
replicate(10, rLaplaceGpd(1, location, scale, threshold, tail_scale, tail_shape))
#>  [1] -0.3746526 -3.1030131  6.1397552 -2.5908396  0.5485603  0.1859425
#>  [7]  0.4937078  0.7970026  1.8303061  1.9040440
```
