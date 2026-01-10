# Amoroso with a GPD tail

Splices a generalized Pareto distribution (GPD) above a threshold
`threshold` onto a single Amoroso bulk with parameters `loc`, `scale`,
`shape1`, and `shape2`.

## Usage

``` r
dAmorosoGpd(
  x,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape,
  log = 0
)

pAmorosoGpd(
  q,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rAmorosoGpd(n, loc, scale, shape1, shape2, threshold, tail_scale, tail_shape)

qAmorosoGpd(
  p,
  loc,
  scale,
  shape1,
  shape2,
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

- loc:

  Numeric scalar location parameter of the Amoroso bulk.

- scale:

  Numeric scalar scale parameter of the Amoroso bulk.

- shape1:

  Numeric scalar first Amoroso shape parameter.

- shape2:

  Numeric scalar second Amoroso shape parameter.

- threshold:

  Numeric scalar threshold at which the GPD tail is attached.

- tail_scale:

  Numeric scalar GPD scale parameter; must be positive.

- tail_shape:

  Numeric scalar GPD shape parameter.

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

  Integer giving the number of draws. The RNG implementation supports
  `n = 1`.

- p:

  Numeric scalar probability in \\(0,1)\\ for the quantile function.

- tol:

  Numeric tolerance for numerical inversion in `qAmorosoGpd`.

- maxiter:

  Maximum iterations for numerical inversion in `qAmorosoGpd`.

## Value

Spliced density/CDF/RNG functions return numeric scalars. `qAmorosoGpd`
returns a numeric vector with the same length as `p`.

## Functions

- `dAmorosoGpd()`: Density Function of Amoroso Distribution with GPD
  Tail

- `pAmorosoGpd()`: Cumulative Distribution Function of Amoroso
  Distribution with GPD Tail

- `rAmorosoGpd()`: Random Generation for Amoroso Distribution with GPD
  Tail

- `qAmorosoGpd()`: Quantile Function of Amoroso Distribution with GPD
  Tail

## Examples

``` r
loc <- 0
scale <- 1.5
shape1 <- 2
shape2 <- 1.2
threshold <- 3
tail_scale <- 1.0
tail_shape <- 0.2

dAmorosoGpd(4.0, loc, scale, shape1, shape2,
           threshold, tail_scale, tail_shape, log = 0)
#> [1] 1e-300
pAmorosoGpd(4.0, loc, scale, shape1, shape2,
           threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
#> [1] 0.8667957
qAmorosoGpd(0.50, loc, scale, shape1, shape2,
           threshold, tail_scale, tail_shape)
#> [1] 2.309366
qAmorosoGpd(0.95, loc, scale, shape1, shape2,
           threshold, tail_scale, tail_shape)
#> [1] 5.298959
replicate(10, rAmorosoGpd(1, loc, scale, shape1, shape2,
                         threshold, tail_scale, tail_shape))
#>  [1] 1.2615281 0.7103682 6.3013993 3.0468416 3.2229290 4.5536784 2.4216707
#>  [8] 4.3138464 1.3671128 2.8536680
```
