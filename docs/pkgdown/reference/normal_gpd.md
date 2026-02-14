# Normal with a GPD tail

Splices a generalized Pareto distribution (GPD) above `threshold` onto a
single Normal bulk.

## Usage

``` r
dNormGpd(x, mean, sd, threshold, tail_scale, tail_shape, log = 0)

pNormGpd(
  q,
  mean,
  sd,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rNormGpd(n, mean, sd, threshold, tail_scale, tail_shape)

qNormGpd(
  p,
  mean,
  sd,
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

- mean:

  Numeric scalar mean parameter for the Normal bulk.

- sd:

  Numeric scalar standard deviation for the Normal bulk.

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

## Value

Spliced density/CDF/RNG functions return numeric scalars. `qNormGpd`
returns a numeric vector with the same length as `p`.

## Functions

- `dNormGpd()`: Normal + GPD tail density

- `pNormGpd()`: Normal + GPD tail distribution function

- `rNormGpd()`: Normal + GPD tail random generation

- `qNormGpd()`: Normal + GPD tail quantile function

## Examples

``` r
mean <- 0.5
sd <- 1.0
threshold <- 2
tail_scale <- 1.0
tail_shape <- 0.2

dNormGpd(3.0, mean, sd, threshold, tail_scale, tail_shape, log = FALSE)
#> [1] 0.0223736
pNormGpd(3.0, mean, sd, threshold, tail_scale, tail_shape,
        lower.tail = TRUE, log.p = FALSE)
#> [1] 0.9731517
qNormGpd(0.50, mean, sd, threshold, tail_scale, tail_shape)
#> [1] 0.5
qNormGpd(0.95, mean, sd, threshold, tail_scale, tail_shape)
#> [1] 2.29835
replicate(10, rNormGpd(1, mean, sd, threshold, tail_scale, tail_shape))
#>  [1]  0.4221132  0.2305136 -0.3302143  2.9633691 -0.2517233  1.7487550
#>  [7]  0.7897745 -0.7741013  1.9601102  1.5545030
```
