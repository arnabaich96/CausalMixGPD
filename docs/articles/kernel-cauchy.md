# Kernel: Cauchy

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
scale <- c(0.7, 1.0)

x <- 0.2
p <- 0.9

dCauchyMix(x, w = w, location = location, scale = scale, log = FALSE)
#> [1] 0.2596958
pCauchyMix(x, w = w, location = location, scale = scale, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.5786132
qCauchyMix(p, w = w, location = location, scale = scale)
#> [1] 2.759006
```

## GPD tail support

Cauchy does not support `GPD = TRUE` in the current registry.

## Example: model specification

``` r
set.seed(1)
y <- abs(rcauchy(30)) + 0.1

bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "cauchy",
  GPD = FALSE,
  J = 6
)

bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "cauchy"
#> 
#> $GPD
#> [1] FALSE
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
