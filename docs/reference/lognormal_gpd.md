# Lognormal with a Gpd tail

Splices a generalized Pareto distribution (Gpd) above `threshold` onto a
single Lognormal bulk with parameters `meanlog` and `sdlog`. Base
Lognormal functions are taken from stats.

## Usage

``` r
dLognormalGpd(x, meanlog, sdlog, threshold, tail_scale, tail_shape, log = 0)

pLognormalGpd(
  q,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rLognormalGpd(n, meanlog, sdlog, threshold, tail_scale, tail_shape)

qLognormalGpd(
  p,
  meanlog,
  sdlog,
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

- meanlog:

  Numeric scalar log-mean parameter for the Lognormal bulk.

- sdlog:

  Numeric scalar log-standard deviation for the Lognormal bulk.

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

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qLognormalGpd` returns a numeric vector with the same length as `p`.

## Functions

- `dLognormalGpd()`: Lognormal + Gpd tail density

- `pLognormalGpd()`: Lognormal + Gpd tail distribution function

- `rLognormalGpd()`: Lognormal + Gpd tail random generation

- `qLognormalGpd()`: Lognormal + Gpd tail quantile function

## Examples

``` r
meanlog <- 0.4
sdlog <- 0.35
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dLognormalGpd(4.0, meanlog, sdlog, threshold, tail_scale, tail_shape, log = FALSE)
#> [1] 0.007654624
pLognormalGpd(4.0, meanlog, sdlog, threshold, tail_scale, tail_shape,
             lower.tail = TRUE, log.p = FALSE)
#> [1] 0.9915799
qLognormalGpd(0.50, meanlog, sdlog, threshold, tail_scale, tail_shape)
#> [1] 1.491825
qLognormalGpd(0.95, meanlog, sdlog, threshold, tail_scale, tail_shape)
#> [1] 2.65302
replicate(10, rLognormalGpd(1, meanlog, sdlog, threshold, tail_scale, tail_shape))
#>  [1] 1.1149317 0.9059939 1.5881729 1.6711589 1.0603531 2.6230252 1.7190734
#>  [8] 1.6648508 2.2918030 2.1176562
```
