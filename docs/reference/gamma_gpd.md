# Gamma with a GPD tail

Splices a generalized Pareto distribution (GPD) above `threshold` onto a
single Gamma bulk distribution parameterized by scale `scale` and shape
`shape`. The bulk probability at the threshold,
\\F\_{bulk}(threshold)\\, scales the tail mass so the overall CDF is
proper.

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

Spliced density/CDF/RNG functions return numeric scalars. `qGammaGpd`
returns a numeric vector with the same length as `p`.

## Functions

- `dGammaGpd()`: Gamma + GPD tail density

- `pGammaGpd()`: Gamma + GPD tail distribution function

- `rGammaGpd()`: Gamma + GPD tail random generation

- `qGammaGpd()`: Gamma + GPD tail quantile function

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
#> [1] 9.66231e-301
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
#>  [1] 3.842909 3.122702 4.716731 3.739240 3.091682 3.173512 4.134970 3.200943
#>  [9] 3.462883 4.803602
```
