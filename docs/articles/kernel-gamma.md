# Kernel: Gamma

## Parameters

| Parameter | Meaning | Support  |
|-----------|---------|----------|
| `shape`   | shape   | positive |
| `scale`   | scale   | positive |

## Default priors (no covariates)

- `shape` ~ Gamma(2, 1)
- `scale` ~ Gamma(2, 1)

## Example: mixture functions

``` r
library(DPmixGPD)
library(nimble)
use_cached_fit <- TRUE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_small <- readRDS(.fit_path("fit_small.rds"))
w <- c(0.4, 0.6)
shape <- c(2.0, 4.0)
scale <- c(0.5, 0.8)

x <- 1.0
p <- 0.9

dGammaMix(x, w = w, shape = shape, scale = scale, log = FALSE)
#> [1] 0.2864839
pGammaMix(x, w = w, shape = shape, scale = scale, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.2605591
qGammaMix(p, w = w, shape = shape, scale = scale)
#> [1] 4.671995
```

## Example: with GPD tail

``` r
threshold <- 1.1
tail_scale <- 0.9
tail_shape <- 0.2

dGammaMixGpd(x, w = w, shape = shape, scale = scale,
             threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 0.2864839
qGammaMixGpd(0.99, w = w, shape = shape, scale = scale,
             threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 7.158889
```

## Example: model specification

``` r
set.seed(1)
y <- rgamma(30, shape = 2, scale = 0.8)
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "gamma",
  GPD = TRUE,
  J = 6
)

bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "gamma"
#> 
#> $GPD
#> [1] TRUE
#> 
#> $has_X
#> [1] FALSE
#> 
#> $N
#> [1] 30
#> 
#> $P
#> [1] 0
#> 
#> $components
#> [1] 6
```
