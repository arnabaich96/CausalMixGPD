# Cauchy distribution

Base Cauchy distribution functions implemented as nimbleFunctions so
they can be used in NIMBLE models. Parameterization uses location and
scale (scale \> 0). The `d*`, `p*`, and `q*` functions accept vector
inputs for their first argument and evaluate elementwise; `r*` supports
`n > 1`.

## Usage

``` r
dCauchy(x, location, scale, log = 0)

pCauchy(q, location, scale, lower.tail = 1, log.p = 0)

rCauchy(n, location, scale)

qCauchy(p, location, scale, lower.tail = TRUE, log.p = FALSE)
```

## Arguments

- x:

  Numeric scalar giving the point at which the density is evaluated.

- location:

  Numeric scalar location parameter.

- scale:

  Numeric scalar scale parameter; must be positive.

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

`dCauchy` returns a numeric scalar density; `pCauchy` returns a numeric
scalar CDF; `rCauchy` returns one random draw; `qCauchy` returns a
numeric quantile.

## Functions

- `dCauchy()`: Cauchy density function

- `pCauchy()`: Cauchy distribution function

- `rCauchy()`: Cauchy random generation

- `qCauchy()`: Cauchy quantile function

## Examples

``` r
location <- 0
scale <- 1.5

dCauchy(0.5, location, scale, log = 0)
#> [1] 0.191
pCauchy(0.5, location, scale, lower.tail = 1, log.p = 0)
#> [1] 0.602
qCauchy(0.50, location, scale)
#> [1] 0
qCauchy(0.95, location, scale)
#> [1] 9.47
replicate(10, rCauchy(1, location, scale))
#>  [1] -10.631   5.154  -6.644 153.594   0.550  -2.490   5.129 -12.690   0.454
#> [10]  -1.628
```
