# Amoroso distribution

Base Amoroso distribution functions as implemented in this package.
Function names and parameterization follow your existing Amoroso
implementation. The `d*`, `p*`, and `q*` functions accept vector inputs
for their first argument and evaluate elementwise; `r*` supports
`n > 1`.

## Usage

``` r
dAmoroso(x, loc, scale, shape1, shape2, log = 0)

pAmoroso(q, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)

qAmoroso(p, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE)

rAmoroso(n, loc, scale, shape1, shape2)
```

## Arguments

- x:

  Numeric scalar giving the point at which the density is evaluated.

- loc:

  Numeric scalar location parameter.

- scale:

  Numeric scalar scale parameter.

- shape1:

  Numeric scalar first shape parameter.

- shape2:

  Numeric scalar second shape parameter.

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

- p:

  Numeric scalar probability in \\(0,1)\\ for the quantile function.

- n:

  Integer giving the number of draws. For portability inside NIMBLE, the
  RNG implementation supports `n = 1`.

## Value

Density/CDF/RNG functions return numeric scalars. The quantile function
returns a numeric scalar.

## Functions

- `dAmoroso()`: Density Function of Amoroso Distribution

- `pAmoroso()`: Distribution Function of Amoroso Distribution

- `qAmoroso()`: Quantile Function of Amoroso Distribution

- `rAmoroso()`: Sample generating Function of Amoroso Distribution

## Examples

``` r
loc <- 0
scale <- 1.5
shape1 <- 2
shape2 <- 1.2

dAmoroso(1.0, loc, scale, shape1, shape2, log = 0)
#> [1] 0.245
pAmoroso(1.0, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)
#> [1] 0.127
qAmoroso(0.50, loc, scale, shape1, shape2)
#> [1] 2.31
qAmoroso(0.95, loc, scale, shape1, shape2)
#> [1] 5.49
replicate(10, rAmoroso(1, loc, scale, shape1, shape2))
#>  [1] 2.701 1.120 0.268 2.188 2.301 1.586 3.327 3.560 4.365 1.186
```
