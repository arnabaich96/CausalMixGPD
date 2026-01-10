# Kernel: Amoroso

## Parameters

| Parameter | Meaning  | Support  |
|-----------|----------|----------|
| `loc`     | location | real     |
| `scale`   | scale    | positive |
| `shape1`  | shape    | positive |
| `shape2`  | shape    | positive |

## Default priors (no covariates)

- `loc` ~ Normal(0, 5)
- `scale` ~ Gamma(2, 1)
- `shape1` fixed at 1 by default
- `shape2` ~ Gamma(2, 1)

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
w <- c(0.6, 0.4)
loc <- c(0.0, 0.5)
scale <- c(1.0, 1.4)
shape1 <- c(1.0, 1.0)
shape2 <- c(2.0, 3.0)

x <- 1.0
p <- 0.9

dAmorosoMix(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2, log = FALSE)
#> [1] 0.5459161
pAmorosoMix(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
            lower.tail = TRUE, log.p = FALSE)
#> [1] 0.3970851
qAmorosoMix(p, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2)
#> [1] 2.09016
```

## Example: with GPD tail

``` r
threshold <- 1.2
tail_scale <- 0.9
tail_shape <- 0.2

dAmorosoMixGpd(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
               threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 0.5459161
qAmorosoMixGpd(0.99, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
               threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 6.521116
```

## Example: model specification

``` r
set.seed(1)
y <- rgamma(30, shape = 2, scale = 1)

bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "amoroso",
  GPD = TRUE,
  J = 6
)

bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "amoroso"
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
