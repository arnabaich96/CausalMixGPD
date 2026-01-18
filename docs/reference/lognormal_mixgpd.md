# Lognormal mixture with a Gpd tail

Splices a generalized Pareto distribution (Gpd) above `threshold` onto a
Lognormal mixture bulk. Let \\F\_{mix}\\ be the Lognormal mixture CDF.
The spliced CDF is \\F(x)=F\_{mix}(x)\\ for \\x\<threshold\\ and
\\F(x)=F\_{mix}(threshold) + \\1-F\_{mix}(threshold)\\G(x)\\ for \\x\ge
threshold\\, where \\G\\ is the Gpd CDF for exceedances above
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

  Numeric scalar threshold at which the Gpd tail is attached.

- tail_scale:

  Numeric scalar Gpd scale parameter; must be positive.

- tail_shape:

  Numeric scalar Gpd shape parameter.

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
`qLognormalMixGpd` returns a numeric vector with the same length as `p`.

## Details

The density, CDF, and RNG are implemented as `nimbleFunction`s. The
quantile is an R function: it uses numerical inversion in the bulk
region and the closed-form Gpd quantile in the tail.

## Functions

- `dLognormalMixGpd()`: Lognormal mixture + Gpd tail density

- `pLognormalMixGpd()`: Lognormal mixture + Gpd tail distribution
  function

- `rLognormalMixGpd()`: Lognormal mixture + Gpd tail random generation

- `qLognormalMixGpd()`: Lognormal mixture + Gpd tail quantile function

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
#>  [1] 0.6241338 1.0414840 1.6177531 0.3387945 8.5712332 0.8091717 0.6202308
#>  [8] 0.9334205 5.7284051 1.9207468
```
