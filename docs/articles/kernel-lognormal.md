# Kernel: Lognormal

## Parameters

| Parameter | Meaning            | Support  |
|-----------|--------------------|----------|
| `meanlog` | log-scale location | real     |
| `sdlog`   | log-scale spread   | positive |

## Default priors (no covariates)

- `meanlog` ~ Normal(0, 5)
- `sdlog` ~ Gamma(2, 1)

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
meanlog <- c(-0.2, 0.4)
sdlog <- c(0.3, 0.6)

x <- 1.0
p <- 0.9

dLognormalMix(x, w = w, meanlog = meanlog, sdlog = sdlog, log = FALSE)
#> [1] 0.79862
pLognormalMix(x, w = w, meanlog = meanlog, sdlog = sdlog, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.5
qLognormalMix(p, w = w, meanlog = meanlog, sdlog = sdlog)
#> [1] 2.472473
```

## Example: with GPD tail

``` r
threshold <- 1.2
tail_scale <- 0.8
tail_shape <- 0.2

dLognormalMixGpd(x, w = w, meanlog = meanlog, sdlog = sdlog,
                 threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 0.79862
qLognormalMixGpd(0.99, w = w, meanlog = meanlog, sdlog = sdlog,
                 threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 5.442101
```

## Example: model specification

``` r
set.seed(1)
y <- rlnorm(30, meanlog = 0.1, sdlog = 0.4)
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "lognormal",
  GPD = TRUE,
  J = 6
)

bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "lognormal"
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
