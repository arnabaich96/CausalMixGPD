# Kernel: Normal

## Parameters

| Parameter | Meaning  | Support  |
|-----------|----------|----------|
| `mean`    | location | real     |
| `sd`      | scale    | positive |

## Default priors (no covariates)

- `mean` ~ Normal(0, 5)
- `sd` ~ Gamma(2, 1)

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
mean <- c(-1, 1)
sd <- c(0.7, 1.2)

x <- 0.5
p <- 0.9

dNormMix(x, w = w, mean = mean, sd = sd, log = FALSE)
#> [1] 0.2058354
pNormMix(x, w = w, mean = mean, sd = sd, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.5966518
qNormMix(p, w = w, mean = mean, sd = sd)
#> [1] 2.160916
```

## Example: with GPD tail

``` r
threshold <- 0.2
tail_scale <- 1.0
tail_shape <- 0.2

dNormMixGpd(x, w = w, mean = mean, sd = sd,
            threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 1e-300
qNormMixGpd(0.99, w = w, mean = mean, sd = sd,
            threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 5.979779
```

## Example: model specification

``` r
set.seed(1)
y <- abs(rnorm(30)) + 0.1
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  J = 6
)

bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "normal"
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
