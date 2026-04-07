# Gamma mixture with a GPD tail

Spliced bulk-tail family formed by attaching a generalized Pareto tail
to a gamma mixture bulk.

## Usage

``` r
dGammaMixGpd(x, w, shape, scale, threshold, tail_scale, tail_shape, log = 0)

pGammaMixGpd(
  q,
  w,
  shape,
  scale,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rGammaMixGpd(n, w, shape, scale, threshold, tail_scale, tail_shape)

qGammaMixGpd(
  p,
  w,
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

- w:

  Numeric vector of mixture weights of length \\K\\.

- shape, scale:

  Numeric vectors of length \\K\\ giving Gamma shape and scale
  parameters.

- threshold:

  Numeric scalar threshold at which the GPD tail is attached.

- tail_scale:

  Numeric scalar GPD scale parameter; must be positive.

- tail_shape:

  Numeric scalar GPD shape parameter.

- log:

  Logical; if `TRUE`, return the log-density.

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

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qGammaMixGpd()` returns a numeric vector with the same length as `p`.

## Details

The gamma mixture governs the body of the distribution up to the
threshold \\u\\. Beyond \\u\\, only the remaining survival mass is
modeled by the GPD, giving \$\$ f(x) = \left\\ \begin{array}{ll}
f\_{mix}(x), & x \< u, \\ \\1-F\_{mix}(u)\\ g\_{GPD}(x \mid
u,\sigma_u,\xi), & x \ge u. \end{array} \right. \$\$ This is the
positive-support analogue of the normal and lognormal splice families.
Bulk quantiles are still found by numerical inversion, while tail
quantiles use the explicit GPD inverse.

## Functions

- `dGammaMixGpd()`: Gamma mixture + GPD tail density

- `pGammaMixGpd()`: Gamma mixture + GPD tail distribution function

- `rGammaMixGpd()`: Gamma mixture + GPD tail random generation

- `qGammaMixGpd()`: Gamma mixture + GPD tail quantile function

## See also

[`gamma_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mix.md),
[`gamma_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_gpd.md),
[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`gamma_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_lowercase.md),
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

Other gamma kernel families:
[`gamma_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_gpd.md),
[`gamma_mix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gamma_mix.md)

## Examples

``` r
w <- c(0.55, 0.30, 0.15)
scale <- c(1.0, 2.5, 5.0)
shape <- c(2, 4, 6)
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dGammaMixGpd(4.0, w = w, scale = scale, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape, log = 0)
#> [1] 0.1831223
pGammaMixGpd(4.0, w = w, scale = scale, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape, lower.tail = 1, log.p = 0)
#> [1] 0.7985655
qGammaMixGpd(0.50, w = w, scale = scale, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape)
#> [1] 3.085593
qGammaMixGpd(0.95, w = w, scale = scale, shape = shape,
            threshold = threshold, tail_scale = tail_scale,
            tail_shape = tail_shape)
#> [1] 5.767674
replicate(10, rGammaMixGpd(1, w = w, scale = scale, shape = shape,
                          threshold = threshold,
                          tail_scale = tail_scale,
                          tail_shape = tail_shape))
#>  [1]  3.5450129  3.2101338 17.7637911  3.9196863  3.0330340  2.1964567
#>  [7]  3.9645414  3.2099800  4.8952188  0.8944191
```
