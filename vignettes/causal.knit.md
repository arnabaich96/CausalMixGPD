---
title: "Causal modeling quickstart"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Causal modeling quickstart}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else {
  library(DPmixGPD)
}
library(nimble)
use_cached_fit <- FALSE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_causal_small <- NULL
if (use_cached_fit) {
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
}
```


``` r
set.seed(1)
dat <- sim_causal_qte(n = 120)
X <- dat$X
T <- dat$t
y <- dat$y

cb <- build_causal_bundle(
  y = y,
  X = X,
  T = T,
  backend = c("sb", "sb"),
  kernel = c("normal", "normal"),
  GPD = c(FALSE, FALSE),
  J = c(8, 8),
  mcmc_outcome = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1),
  mcmc_ps = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
)
```






