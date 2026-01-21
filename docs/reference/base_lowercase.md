# Lowercase vectorized distribution functions (base kernels)

Vectorized R wrappers for the base distribution functions. These
lowercase versions accept vector inputs for the first argument (`x`,
`q`, or `p`) and return a numeric vector. The `r*` functions support
`n > 1` and return a numeric vector of length `n`.

## Usage

``` r
dgpd(x, threshold, scale, shape, log = FALSE)

pgpd(q, threshold, scale, shape, lower.tail = TRUE, log.p = FALSE)

qgpd(p, threshold, scale, shape, lower.tail = TRUE, log.p = FALSE)

rgpd(n, threshold, scale, shape)

dinvgauss(x, mean, shape, log = FALSE)

pinvgauss(q, mean, shape, lower.tail = TRUE, log.p = FALSE)

qinvgauss(
  p,
  mean,
  shape,
  lower.tail = TRUE,
  log.p = FALSE,
  tol = 1e-10,
  maxiter = 200
)

rinvgauss(n, mean, shape)

damoroso(x, loc, scale, shape1, shape2, log = FALSE)

pamoroso(q, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE)

qamoroso(p, loc, scale, shape1, shape2, lower.tail = TRUE, log.p = FALSE)

ramoroso(n, loc, scale, shape1, shape2)

dcauchy_vec(x, location, scale, log = FALSE)

pcauchy_vec(q, location, scale, lower.tail = TRUE, log.p = FALSE)

qcauchy_vec(p, location, scale, lower.tail = TRUE, log.p = FALSE)

rcauchy_vec(n, location, scale)
```

## Arguments

- x:

  Numeric vector of quantiles.

- threshold, scale, shape, mean, loc, scale, shape1, shape2, location:

  Distribution parameters (scalars).

- log:

  Logical; if `TRUE`, return log-density.

- q:

  Numeric vector of quantiles.

- lower.tail:

  Logical; if `TRUE` (default), probabilities are \\P(X \le x)\\.

- log.p:

  Logical; if `TRUE`, probabilities are on log scale.

- p:

  Numeric vector of probabilities.

- n:

  Integer number of observations to generate.

- tol, maxiter:

  Tolerance and max iterations for numerical inversion.

## Value

Numeric vector of densities, probabilities, quantiles, or random
variates.

## Details

The underlying scalar functions are NIMBLE-compatible nimbleFunctions.
These wrappers are for convenient R-side usage and are not intended for
use inside NIMBLE models.

## Functions

- `dgpd()`: GPD density (vectorized)

- `pgpd()`: GPD distribution function (vectorized)

- `qgpd()`: GPD quantile function (vectorized)

- `rgpd()`: GPD random generation (vectorized)

- `dinvgauss()`: Inverse Gaussian density (vectorized)

- `pinvgauss()`: Inverse Gaussian distribution function (vectorized)

- `qinvgauss()`: Inverse Gaussian quantile function (vectorized)

- `rinvgauss()`: Inverse Gaussian random generation (vectorized)

- `damoroso()`: Amoroso density (vectorized)

- `pamoroso()`: Amoroso distribution function (vectorized)

- `qamoroso()`: Amoroso quantile function (vectorized)

- `ramoroso()`: Amoroso random generation (vectorized)

- `dcauchy_vec()`: Cauchy density (vectorized)

- `pcauchy_vec()`: Cauchy distribution function (vectorized)

- `qcauchy_vec()`: Cauchy quantile function (vectorized)

- `rcauchy_vec()`: Cauchy random generation (vectorized)

## Examples

``` r
# GPD
dgpd(c(1.5, 2.0, 2.5), threshold = 1, scale = 0.8, shape = 0.2)
#> Error in dgpd(c(1.5, 2, 2.5), threshold = 1, scale = 0.8, shape = 0.2): could not find function "dgpd"
pgpd(c(1.5, 2.0), threshold = 1, scale = 0.8, shape = 0.2)
#> Error in pgpd(c(1.5, 2), threshold = 1, scale = 0.8, shape = 0.2): could not find function "pgpd"
qgpd(c(0.5, 0.9), threshold = 1, scale = 0.8, shape = 0.2)
#> Error in qgpd(c(0.5, 0.9), threshold = 1, scale = 0.8, shape = 0.2): could not find function "qgpd"
rgpd(5, threshold = 1, scale = 0.8, shape = 0.2)
#> Error in rgpd(5, threshold = 1, scale = 0.8, shape = 0.2): could not find function "rgpd"

# Inverse Gaussian
dinvgauss(c(1, 2, 3), mean = 2, shape = 5)
#> Error in dinvgauss(c(1, 2, 3), mean = 2, shape = 5): could not find function "dinvgauss"
rinvgauss(5, mean = 2, shape = 5)
#> Error in rinvgauss(5, mean = 2, shape = 5): could not find function "rinvgauss"

# Amoroso
damoroso(c(1, 2), loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2)
#> Error in damoroso(c(1, 2), loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2): could not find function "damoroso"
ramoroso(5, loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2)
#> Error in ramoroso(5, loc = 0, scale = 1.5, shape1 = 2, shape2 = 1.2): could not find function "ramoroso"

# Cauchy
dcauchy_vec(c(-1, 0, 1), location = 0, scale = 1)
#> Error in dcauchy_vec(c(-1, 0, 1), location = 0, scale = 1): could not find function "dcauchy_vec"
rcauchy_vec(5, location = 0, scale = 1)
#> Error in rcauchy_vec(5, location = 0, scale = 1): could not find function "rcauchy_vec"
```
