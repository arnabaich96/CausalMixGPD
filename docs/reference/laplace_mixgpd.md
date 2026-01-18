# Laplace mixture with a Gpd tail

Splices a generalized Pareto distribution (Gpd) above `threshold` onto a
Laplace mixture bulk. The bulk probability at the threshold is used to
scale the tail so that the overall CDF is proper.

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

- tol:

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

- maxiter:

  Integer maximum iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qLaplaceMixGpd` returns a numeric vector with the same length as `p`.

## Functions

- `dLaplaceMixGpd()`: Laplace mixture + Gpd tail density

- `pLaplaceMixGpd()`: Laplace mixture + Gpd tail distribution function

- `rLaplaceMixGpd()`: Laplace mixture + Gpd tail random generation

- `qLaplaceMixGpd()`: Laplace mixture + Gpd tail quantile function

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
#>  [1]  2.3575969 -0.7314386  1.5493416 -1.5075673  1.4471097  1.3451593
#>  [7]  0.4536589  0.4869627 -1.2862912  1.6929401
```
