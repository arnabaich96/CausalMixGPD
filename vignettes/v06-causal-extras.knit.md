---
title: "Causal extras"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Causal extras}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
library(DPmixGPD)
library(nimble)
use_cached_fit <- TRUE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_causal_con <- readRDS(.fit_path("fit_causal_con.rds"))
fit_causal_trt <- readRDS(.fit_path("fit_causal_trt.rds"))
fit_causal_meta <- readRDS(.fit_path("fit_causal_meta.rds"))
fit_causal_small <- list(
  ps_fit = NULL,
  outcome_fit = list(con = fit_causal_con, trt = fit_causal_trt),
  bundle = fit_causal_meta,
  call = NULL
)
class(fit_causal_small) <- "dpmixgpd_causal_fit"
library(ggplot2)
library(dplyr)
```










