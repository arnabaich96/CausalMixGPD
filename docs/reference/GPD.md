# Generalized Pareto distribution

Base generalized Pareto distribution (GPD) for threshold exceedances
above `threshold`. Parameterization uses threshold `threshold`, scale
`scale > 0`, and shape `shape`. The `d*`, `p*`, and `q*` functions
accept vector inputs for their first argument and evaluate elementwise;
`r*` supports `n > 1`.

## Usage

``` r
dGpd(x, threshold, scale, shape, log = 0)

pGpd(q, threshold, scale, shape, lower.tail = 1, log.p = 0)

rGpd(n, threshold, scale, shape)

rGpd_vec(n, threshold, scale, shape)

qGpd(p, threshold, scale, shape, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- x:

  Numeric scalar giving the point at which the density is evaluated.

- threshold:

  Numeric scalar threshold at which the GPD is attached.

- scale:

  Numeric scalar GPD scale parameter; must be positive.

- shape:

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

## Value

`dGpd` returns a numeric scalar density; `pGpd` returns a numeric scalar
CDF; `rGpd` returns one random draw; `qGpd` returns a numeric quantile.

## Functions

- `dGpd()`: Generalized Pareto density function

- `pGpd()`: Generalized Pareto distribution function

- `rGpd()`: Generalized Pareto random generation

- `rGpd_vec()`: Vectorized RNG wrapper (R-only)

- `qGpd()`: Generalized Pareto quantile function

## Examples

``` r
threshold <- 1
tail_scale <- 0.8
tail_shape <- 0.2

dGpd(1.5, threshold, tail_scale, tail_shape, log = 0)
#> [1] 0.6165877
pGpd(1.5, threshold, tail_scale, tail_shape, lower.tail = 1, log.p = 0)
#> [1] 0.445071
qGpd(0.50, threshold, tail_scale, tail_shape)
#> [1] 1.594793
qGpd(0.95, threshold, tail_scale, tail_shape)
#> [1] 4.282257
replicate(10, rGpd(1, threshold, tail_scale, tail_shape))
#>  [1] 2.679374 2.181256 1.383534 1.639107 1.541350 1.083883 1.624845 1.507601
#>  [9] 1.691521 1.933145
```
