# Kernel: Laplace

## Parameters

| Parameter  | Meaning  | Support  |
|------------|----------|----------|
| `location` | location | real     |
| `scale`    | scale    | positive |

## Default priors (no covariates)

- `location` ~ Normal(0, 5)
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
w <- c(0.5, 0.5)
location <- c(-0.5, 0.5)
scale <- c(0.6, 1.1)

x <- 0.2
p <- 0.9

dLaplaceMix(x, w = w, location = location, scale = scale, log = FALSE)
#> [1] 0.3027742
pLaplaceMix(x, w = w, location = location, scale = scale, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.6124743
qLaplaceMix(p, w = w, location = location, scale = scale)
#> [1] 1.595033
```

## Example: with GPD tail (SB only)

``` r
threshold <- 0.3
tail_scale <- 1.0
tail_shape <- 0.2

dLaplaceMixGpd(x, w = w, location = location, scale = scale,
               threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 0.3027742
qLaplaceMixGpd(0.99, w = w, location = location, scale = scale,
               threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 5.52388
```

## Example: model specification

``` r
set.seed(1)
y <- rexp(30)

bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "laplace",
  GPD = TRUE,
  J = 6
)

bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "laplace"
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
