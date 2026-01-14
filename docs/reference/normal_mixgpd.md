# Normal mixture with a Gpd tail

Splices a generalized Pareto distribution (Gpd) above `threshold` onto a
Normal mixture bulk. The bulk probability at the threshold is used to
scale the tail so that the overall CDF is proper.

## Usage

``` r
dNormMixGpd(x, w, mean, sd, threshold, tail_scale, tail_shape, log = 0)

pNormMixGpd(
  q,
  w,
  mean,
  sd,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rNormMixGpd(n, w, mean, sd, threshold, tail_scale, tail_shape)

qNormMixGpd(
  p,
  w,
  mean,
  sd,
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

- mean, sd:

  Numeric vectors of length \\K\\ giving component means and standard
  deviations.

- threshold:

  Numeric scalar threshold at which the Gpd tail is attached.

- tail_scale:

  Numeric scalar Gpd scale parameter; must be positive.

- tail_shape:

  Numeric scalar Gpd shape parameter.

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

Spliced density/CDF/RNG functions return numeric scalars. `qNormMixGpd`
returns a numeric vector with the same length as `p`.

## Functions

- `dNormMixGpd()`: Normal mixture + Gpd tail density

- `pNormMixGpd()`: Normal mixture + Gpd tail distribution function

- `rNormMixGpd()`: Normal mixture + Gpd tail random generation

- `qNormMixGpd()`: Normal mixture + Gpd tail quantile function

## Examples

``` r
w <- c(0.60, 0.25, 0.15)
mean <- c(-1, 0.5, 2.0)
sd <- c(1.0, 0.7, 1.3)
threshold <- 2
tail_scale <- 1.0
tail_shape <- 0.2

dNormMixGpd(3.0, w, mean, sd, threshold, tail_scale, tail_shape, log = FALSE)
#> [1] 1e-300
pNormMixGpd(3.0, w, mean, sd, threshold, tail_scale, tail_shape,
           lower.tail = TRUE, log.p = FALSE)
#> [1] 0.9679199
qNormMixGpd(0.50, w, mean, sd, threshold, tail_scale, tail_shape)
#> [1] -0.2713211
qNormMixGpd(0.95, w, mean, sd, threshold, tail_scale, tail_shape)
#> [1] 2.490405
replicate(10, rNormMixGpd(1, w, mean, sd, threshold, tail_scale, tail_shape))
#>  [1]  2.7900057  1.3379804  3.0202973 -1.3517409 -2.0832023  2.1104130
#>  [7] -0.5494130 -0.2205110  2.0889979 -0.2118014
```
