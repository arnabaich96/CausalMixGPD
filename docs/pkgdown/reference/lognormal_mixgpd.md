# Lognormal mixture with a GPD tail

Spliced bulk-tail family formed by attaching a generalized Pareto tail
to a lognormal mixture bulk. Let \\F\_{mix}\\ be the Lognormal mixture
CDF. The spliced CDF is \\F(x)=F\_{mix}(x)\\ for \\x\<threshold\\ and
\\F(x)=F\_{mix}(threshold) + \\1-F\_{mix}(threshold)\\G(x)\\ for \\x\ge
threshold\\, where \\G\\ is the GPD CDF for exceedances above
`threshold`.

## Usage

``` r
dLognormalMixGpd(
  x,
  w,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  log = 0
)

pLognormalMixGpd(
  q,
  w,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rLognormalMixGpd(n, w, meanlog, sdlog, threshold, tail_scale, tail_shape)

qLognormalMixGpd(
  p,
  w,
  meanlog,
  sdlog,
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

- meanlog, sdlog:

  Numeric vectors of length \\K\\ giving component log-means and
  log-standard deviations.

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
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html) in quantile
  inversion.

- maxiter:

  Integer maximum number of iterations for
  [`stats::uniroot`](https://rdrr.io/r/stats/uniroot.html).

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qLognormalMixGpd()` returns a numeric vector with the same length as
`p`.

## Details

The density, CDF, and RNG are implemented as `nimbleFunction`s. The
quantile is an R function: it uses numerical inversion in the bulk
region and the closed-form GPD quantile in the tail.

Let \\F\_{mix}\\ be the lognormal-mixture CDF and let \\u\\ denote the
threshold. The splice uses the bulk law below \\u\\ and attaches a GPD
to the residual survival mass above \\u\\. The density therefore becomes
\$\$ f(x) = \left\\ \begin{array}{ll} f\_{mix}(x), & x \< u, \\
\\1-F\_{mix}(u)\\ g\_{GPD}(x \mid u,\sigma_u,\xi), & x \ge u.
\end{array} \right. \$\$ The quantile is computed piecewise: bulk
quantiles are obtained numerically from the mixture CDF, whereas tail
quantiles use the closed-form GPD inverse after rescaling the upper-tail
probability.

## Functions

- `dLognormalMixGpd()`: Lognormal mixture + GPD tail density

- `pLognormalMixGpd()`: Lognormal mixture + GPD tail distribution
  function

- `rLognormalMixGpd()`: Lognormal mixture + GPD tail random generation

- `qLognormalMixGpd()`: Lognormal mixture + GPD tail quantile function

## See also

[`lognormal_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mix.md),
[`lognormal_gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_gpd.md),
[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`lognormal_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_lowercase.md),
[`dpmgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/dpmgpd.md).

Other lognormal kernel families:
[`lognormal_gpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_gpd.md),
[`lognormal_mix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mix.md)

## Examples

``` r
w <- c(0.60, 0.25, 0.15)
meanlog <- c(-0.2, 0.6, 1.2)
sdlog <- c(0.4, 0.3, 0.5)
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dLognormalMixGpd(4.0, w = w, meanlog = meanlog, sdlog = sdlog,
                threshold = threshold, tail_scale = tail_scale,
                tail_shape = tail_shape, log = FALSE)
#> [1] 0.03315338
pLognormalMixGpd(4.0, w = w, meanlog = meanlog, sdlog = sdlog,
                threshold = threshold, tail_scale = tail_scale,
                tail_shape = tail_shape, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.9635313
qLognormalMixGpd(0.50, w = w, meanlog = meanlog, sdlog = sdlog,
                threshold = threshold, tail_scale = tail_scale,
                tail_shape = tail_shape)
#> [1] 1.151134
qLognormalMixGpd(0.95, w = w, meanlog = meanlog, sdlog = sdlog,
                threshold = threshold, tail_scale = tail_scale,
                tail_shape = tail_shape)
#> [1] 3.663602
replicate(10, rLognormalMixGpd(1, w = w, meanlog = meanlog, sdlog = sdlog,
                              threshold = threshold,
                              tail_scale = tail_scale,
                              tail_shape = tail_shape))
#>  [1] 1.1334015 3.1151530 1.2128602 1.4196270 0.7637399 1.4776769 0.5542335
#>  [8] 5.9072094 0.6781621 2.6326862
```
