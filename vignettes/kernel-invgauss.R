## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.path = "cache/kernel-invgauss-",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library(DPmixGPD)
library(nimble)
use_cached_fit <- TRUE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_small <- readRDS(.fit_path("fit_small.rds"))
w <- c(0.3, 0.7)
mean <- c(1.0, 2.0)
shape <- c(2.0, 3.0)

x <- 1.0
p <- 0.9

dInvGaussMix(x, w = w, mean = mean, shape = shape, log = FALSE)
pInvGaussMix(x, w = w, mean = mean, shape = shape, lower.tail = TRUE, log.p = FALSE)
qInvGaussMix(p, w = w, mean = mean, shape = shape)

## -----------------------------------------------------------------------------
threshold <- 1.2
tail_scale <- 0.8
tail_shape <- 0.2

dInvGaussMixGpd(x, w = w, mean = mean, shape = shape,
                threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
qInvGaussMixGpd(0.99, w = w, mean = mean, shape = shape,
                threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)

## -----------------------------------------------------------------------------
set.seed(1)
y <- rInvGauss(30, mean = 1.5, shape = 2.5)
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "invgauss",
  GPD = TRUE,
  J = 6
)

bundle$spec$meta

