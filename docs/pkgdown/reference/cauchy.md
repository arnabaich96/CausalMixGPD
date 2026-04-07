# Cauchy distribution

Scalar Cauchy utilities implemented for NIMBLE compatibility. These
functions support symmetric heavy-tailed kernels on the real line and
feed directly into the finite Cauchy mixture family.

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

`dCauchy()` returns a numeric scalar density, `pCauchy()` returns a
numeric scalar CDF, `rCauchy()` returns one random draw, and `qCauchy()`
returns a numeric quantile.

## Details

The density is \$\$ f(x) = \frac{1}{\pi s \\1 + ((x - \ell)/s)^2\\},
\$\$ where `location = ell` and `scale = s > 0`.

These uppercase NIMBLE-compatible functions are scalar (`x`/`q` and
`n = 1`). For vectorized R usage, use
[`base_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md).

The Cauchy law is a stable heavy-tailed distribution with undefined mean
and variance. That is why the package allows the Cauchy kernel only as a
bulk distribution and deliberately does not pair it with GPD tails in
the kernel registry. For predictive summaries, ordinary means are not
available under Cauchy kernels; tail-robust summaries such as medians,
quantiles, survival curves, and restricted means remain well defined.

The distribution function is \$\$ F(x) = \frac{1}{2} +
\frac{1}{\pi}\arctan\left(\frac{x-\ell}{s}\right), \$\$ and the quantile
is the corresponding inverse \$\$ Q(p) = \ell + s \tan\\\pi(p-1/2)\\.
\$\$

## Functions

- `dCauchy()`: Cauchy density function

- `pCauchy()`: Cauchy distribution function

- `rCauchy()`: Cauchy random generation

- `qCauchy()`: Cauchy quantile function

## See also

[`cauchy_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/cauchy_mix.md),
[`base_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/base_lowercase.md),
[`kernel_support_table()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/kernel_support_table.md).

Other base bulk distributions:
[`InvGauss`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/InvGauss.md),
[`amoroso`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/amoroso.md)

## Examples

``` r
location <- 0
scale <- 1.5

dCauchy(0.5, location, scale, log = 0)
#> [1] 0.1909859
pCauchy(0.5, location, scale, lower.tail = 1, log.p = 0)
#> [1] 0.6024164
qCauchy(0.50, location, scale)
#> [1] 0
qCauchy(0.95, location, scale)
#> [1] 9.470627
replicate(10, rCauchy(1, location, scale))
#>  [1]   0.5499148  -2.4904496   5.1293918 -12.6901149   0.4540105  -1.6280435
#>  [7]   4.9474819   2.3452264   1.0885857  -1.8131847
```
