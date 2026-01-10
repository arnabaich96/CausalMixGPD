# Gamma mixture with a GPD tail

Splices a generalized Pareto distribution (GPD) above `threshold` onto a
Gamma mixture bulk. The bulk probability at the threshold is used to
scale the tail so that the overall CDF is proper.

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

  Numeric vectors of length \\K\\ giving Gamma shapes and rates.

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

Spliced density/CDF/RNG functions return numeric scalars. `qGammaMixGpd`
returns a numeric vector with the same length as `p`.

## Functions

- `dGammaMixGpd()`: Gamma mixture + GPD tail density

- `pGammaMixGpd()`: Gamma mixture + GPD tail distribution function

- `rGammaMixGpd()`: Gamma mixture + GPD tail random generation

- `qGammaMixGpd()`: Gamma mixture + GPD tail quantile function

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
#> [1] 5.49395e-301
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
#>  [1] 24.8540406  3.7568397  3.7019852  5.1352274  0.6611549  7.1150727
#>  [7]  5.5014902  3.4546414  5.0240218  3.0353935
```
