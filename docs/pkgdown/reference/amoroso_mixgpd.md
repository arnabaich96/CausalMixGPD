# Amoroso mixture with a GPD tail

This family splices a generalized Pareto distribution (GPD) above a
threshold `threshold` onto an Amoroso mixture bulk. Let \\F\_{mix}\\
denote the Amoroso mixture CDF. The spliced CDF is \\F(x)=F\_{mix}(x)\\
for \\x\<threshold\\ and \\F(x)=F\_{mix}(threshold) +
\left\\1-F\_{mix}(threshold)\right\\G(x)\\ for \\x\ge threshold\\, where
\\G\\ is the GPD CDF for exceedances above `threshold`.

## Usage

``` r
dAmorosoMixGpd(
  x,
  w,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape,
  log = 0
)

pAmorosoMixGpd(
  q,
  w,
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

rAmorosoMixGpd(
  n,
  w,
  loc,
  scale,
  shape1,
  shape2,
  threshold,
  tail_scale,
  tail_shape
)

qAmorosoMixGpd(
  p,
  w,
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

- w:

  Numeric vector of mixture weights of length \\K\\. The functions treat
  the weights as non-negative and normalize them internally when needed.

- loc:

  Numeric vector of length \\K\\ giving component locations.

- scale:

  Numeric vector of length \\K\\ giving component scales.

- shape1:

  Numeric vector of length \\K\\ giving the first Amoroso shape
  parameter for each component.

- shape2:

  Numeric vector of length \\K\\ giving the second Amoroso shape
  parameter for each component.

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

  Integer giving the number of draws. For portability inside NIMBLE, the
  RNG implementation supports `n = 1`.

- p:

  Numeric scalar probability in \\(0,1)\\ for the quantile function.

- tol:

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html) in quantile
  inversion.

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qAmorosoMixGpd` returns a numeric vector with the same length as `p`.

## Details

The density, CDF, and RNG are implemented as `nimbleFunction`s for use
in NIMBLE models. The quantile function is an R function that uses
numerical inversion in the bulk region and the closed-form GPD quantile
in the tail region.

## Functions

- `dAmorosoMixGpd()`: Density Function of Amoroso Mixture Distribution
  with GPD Tail

- `pAmorosoMixGpd()`: Cumulative Distribution Function of Amoroso
  Mixture Distribution with GPD Tail

- `rAmorosoMixGpd()`: Random Generation for Amoroso Mixture Distribution
  with GPD Tail

- `qAmorosoMixGpd()`: Quantile Function of Amoroso Mixture Distribution
  with GPD Tail

## Examples

``` r
w <- c(0.60, 0.25, 0.15)
loc <- c(0, 1, 2)
scale <- c(1.0, 1.2, 1.6)
shape1 <- c(2, 4, 6)
shape2 <- c(1.0, 1.2, 1.5)
threshold <- 3
tail_scale <- 1.0
tail_shape <- 0.2

dAmorosoMixGpd(4.0, w, loc, scale, shape1, shape2,
              threshold, tail_scale, tail_shape, log = 0)
#> [1] 0.1642462
pAmorosoMixGpd(4.0, w, loc, scale, shape1, shape2,
              threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
#> [1] 0.8029046
qAmorosoMixGpd(0.50, w, loc, scale, shape1, shape2,
              threshold, tail_scale, tail_shape)
#> [1] 2.929829
qAmorosoMixGpd(0.95, w, loc, scale, shape1, shape2,
              threshold, tail_scale, tail_shape)
#> [1] 5.893917
replicate(10, rAmorosoMixGpd(1, w, loc, scale, shape1, shape2,
                            threshold, tail_scale, tail_shape))
#>  [1] 1.347846 1.877813 3.510209 4.007538 1.615543 3.539162 2.628302 3.164018
#>  [9] 5.269986 3.283937
```
