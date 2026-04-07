# Normal mixture with a GPD tail

Spliced bulk-tail family formed by attaching a generalized Pareto tail
to a normal mixture bulk. This matches the structure used by the
package's normal `mixgpd` kernels.

## Usage

``` r
dNormMixGpd(x, w, mean, sd, threshold, tail_scale, tail_shape, log = 0)

pNormMixGpd(
  q,
  w,
  mean,
  sd,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rNormMixGpd(n, w, mean, sd, threshold, tail_scale, tail_shape)

qNormMixGpd(
  p,
  w,
  mean,
  sd,
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

- mean, sd:

  Numeric vectors of length \\K\\ giving component means and standard
  deviations.

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

  Numeric scalar tolerance passed to
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qNormMixGpd()` returns a numeric vector with the same length as `p`.

## Details

If \\F\_{mix}\\ denotes the normal-mixture CDF, the spliced CDF is \$\$
F(x) = \left\\ \begin{array}{ll} F\_{mix}(x), & x \< u, \\ F\_{mix}(u) +
\\1 - F\_{mix}(u)\\ G(x), & x \ge u, \end{array} \right. \$\$ where
`threshold = u` and \\G\\ is the GPD CDF.

The construction keeps the normal mixture unchanged below the threshold
\\u\\ and replaces the upper tail by a generalized Pareto exceedance
model. Writing \\F\_{mix}(u)=p_u\\, the spliced density is \$\$ f(x) =
\left\\ \begin{array}{ll} f\_{mix}(x), & x \< u, \\ \\1-p_u\\ g\_{GPD}(x
\mid u,\sigma_u,\xi), & x \ge u. \end{array} \right. \$\$ This
formulation preserves total probability because the GPD is attached only
to the residual survival mass above the bulk threshold.

The quantile is piecewise. If \\p \le p_u\\, `qNormMixGpd()` inverts the
bulk mixture CDF; otherwise it rescales the tail probability to
\\(p-p_u)/(1-p_u)\\ and applies the closed-form GPD quantile. That same
piecewise logic is what the fitted-model prediction code uses draw by
draw.

## Functions

- `dNormMixGpd()`: Normal mixture + GPD tail density

- `pNormMixGpd()`: Normal mixture + GPD tail distribution function

- `rNormMixGpd()`: Normal mixture + GPD tail random generation

- `qNormMixGpd()`: Normal mixture + GPD tail quantile function

## See also

[`normal_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_mix.md),
[`normal_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_gpd.md),
[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`normal_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_lowercase.md),
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

Other normal kernel families:
[`normal_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_gpd.md),
[`normal_mix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/normal_mix.md)

## Examples

``` r
w <- c(0.60, 0.25, 0.15)
mean <- c(-1, 0.5, 2.0)
sd <- c(1.0, 0.7, 1.3)
threshold <- 2
tail_scale <- 1.0
tail_shape <- 0.2

dNormMixGpd(3.0, w, mean, sd, threshold, tail_scale, tail_shape, log = FALSE)
#> [1] 0.0267334
pNormMixGpd(3.0, w, mean, sd, threshold, tail_scale, tail_shape,
           lower.tail = TRUE, log.p = FALSE)
#> [1] 0.9679199
qNormMixGpd(0.50, w, mean, sd, threshold, tail_scale, tail_shape)
#> [1] -0.2713211
qNormMixGpd(0.95, w, mean, sd, threshold, tail_scale, tail_shape)
#> [1] 2.490405
replicate(10, rNormMixGpd(1, w, mean, sd, threshold, tail_scale, tail_shape))
#>  [1]  0.70740840 -1.20709960 -0.29636178 -1.17680354  3.26036430 -1.06133771
#>  [7]  1.29417746 -1.04187750 -0.09906829  2.03944294
```
