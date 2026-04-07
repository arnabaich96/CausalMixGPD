# Lognormal with a GPD tail

Spliced family obtained by attaching a generalized Pareto tail above
`threshold` to a single lognormal bulk.

## Usage

``` r
dLognormalGpd(x, meanlog, sdlog, threshold, tail_scale, tail_shape, log = 0)

pLognormalGpd(
  q,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = 1,
  log.p = 0
)

rLognormalGpd(n, meanlog, sdlog, threshold, tail_scale, tail_shape)

qLognormalGpd(
  p,
  meanlog,
  sdlog,
  threshold,
  tail_scale,
  tail_shape,
  lower.tail = TRUE,
  log.p = FALSE
)
```

## Arguments

- x:

  Numeric scalar giving the point at which the density is evaluated.

- meanlog:

  Numeric scalar log-mean parameter for the Lognormal bulk.

- sdlog:

  Numeric scalar log-standard deviation for the Lognormal bulk.

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

## Value

Spliced density/CDF/RNG functions return numeric scalars.
`qLognormalGpd()` returns a numeric vector with the same length as `p`.

## Details

This is the single-lognormal counterpart of
[`lognormal_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mixgpd.md).
If \\F\_{LN}(u)\\ denotes the bulk probability below the threshold, then
the spliced density is \$\$ f(x) = \left\\ \begin{array}{ll} f\_{LN}(x
\mid \mu,\sigma), & x \< u, \\ \\1-F\_{LN}(u)\\ g\_{GPD}(x \mid
u,\sigma_u,\xi), & x \ge u. \end{array} \right. \$\$ The ordinary mean
is finite only when the GPD tail has \\\xi \< 1\\; otherwise the package
requires restricted means or quantiles for tail-robust inference.

## Functions

- `dLognormalGpd()`: Lognormal + GPD tail density

- `pLognormalGpd()`: Lognormal + GPD tail distribution function

- `rLognormalGpd()`: Lognormal + GPD tail random generation

- `qLognormalGpd()`: Lognormal + GPD tail quantile function

## See also

[`lognormal_mix()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mix.md),
[`lognormal_mixgpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mixgpd.md),
[`gpd()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/gpd.md),
[`lognormal_lowercase()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_lowercase.md).

Other lognormal kernel families:
[`lognormal_mix`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mix.md),
[`lognormal_mixgpd`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/lognormal_mixgpd.md)

## Examples

``` r
meanlog <- 0.4
sdlog <- 0.35
threshold <- 3
tail_scale <- 0.9
tail_shape <- 0.2

dLognormalGpd(4.0, meanlog, sdlog, threshold, tail_scale, tail_shape, log = FALSE)
#> [1] 0.007654624
pLognormalGpd(4.0, meanlog, sdlog, threshold, tail_scale, tail_shape,
             lower.tail = TRUE, log.p = FALSE)
#> [1] 0.9915799
qLognormalGpd(0.50, meanlog, sdlog, threshold, tail_scale, tail_shape)
#> [1] 1.491825
qLognormalGpd(0.95, meanlog, sdlog, threshold, tail_scale, tail_shape)
#> [1] 2.65302
replicate(10, rLognormalGpd(1, meanlog, sdlog, threshold, tail_scale, tail_shape))
#>  [1] 1.682650 1.709019 3.140103 1.949344 2.430002 1.112910 2.155999 1.991381
#>  [9] 1.406004 1.409673
```
