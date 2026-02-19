# Amoroso mixture distribution

A finite mixture of Amoroso components is often a convenient bulk model
when the response is positive and right-skewed. These functions evaluate
and simulate from \\\sum_j w_j f\_{Amoroso}(\cdot\mid loc_j, scale_j,
shape1_j, shape2_j)\\.

## Usage

``` r
dAmorosoMix(x, w, loc, scale, shape1, shape2, log = 0)

pAmorosoMix(q, w, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)

rAmorosoMix(n, w, loc, scale, shape1, shape2)

qAmorosoMix(
  p,
  w,
  loc,
  scale,
  shape1,
  shape2,
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

  Numeric vector of length \\K\\ giving component scales. Negative
  values flip support for the corresponding component.

- shape1:

  Numeric vector of length \\K\\ giving the first Amoroso shape
  parameter for each component.

- shape2:

  Numeric vector of length \\K\\ giving the second Amoroso shape
  parameter for each component.

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

Mixture density/CDF/RNG functions return numeric scalars. `qAmorosoMix`
returns a numeric vector with the same length as `p`.

## Details

The density, CDF, and RNG are implemented as `nimbleFunction`s so they
can be called from NIMBLE models. The quantile function is provided as
an R function and is computed by numerical inversion of the mixture CDF.
These uppercase NIMBLE-compatible functions are scalar (`x`/`q` and
`n = 1`). For vectorized R usage (including `n > 1`), use
[`amoroso_lowercase`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso_lowercase.md).

## Functions

- `dAmorosoMix()`: Density Function of Amoroso Mixture Distribution

- `pAmorosoMix()`: Cumulative Distribution Function of Amoroso Mixture
  Distribution

- `rAmorosoMix()`: Random Generation for Amoroso Mixture Distribution

- `qAmorosoMix()`: Quantile Function of Amoroso Mixture Distribution

## Examples

``` r
w <- c(0.60, 0.25, 0.15)
loc <- c(0, 1, 2)
scale <- c(1.0, 1.2, 1.6)
shape1 <- c(2, 4, 6)
shape2 <- c(1.0, 1.2, 1.5)

dAmorosoMix(2.0, w, loc, scale, shape1, shape2, log = 0)
#> [1] 0.1717337
pAmorosoMix(2.0, w, loc, scale, shape1, shape2, lower.tail = 1, log.p = 0)
#> [1] 0.3587001
qAmorosoMix(0.50, w, loc, scale, shape1, shape2)
#> [1] 2.929829
qAmorosoMix(0.95, w, loc, scale, shape1, shape2)
#> [1] 8.017293
replicate(10, rAmorosoMix(1, w, loc, scale, shape1, shape2))
#>  [1] 1.1390523 1.7812916 2.8622365 2.5033891 1.2467692 3.6989854 1.1954265
#>  [8] 0.8008628 7.2399839 1.0390335
```
