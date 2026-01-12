---
title: "Backends: CRP vs SB"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Backends: CRP vs SB}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
# Load into knit global; fall back to package data/ if needed
.kg <- knitr::knit_global()
if (!exists("nc_real200_k2", envir = .kg, inherits = FALSE)) {
  utils::data(list = "nc_real200_k2", package = "DPmixGPD", envir = .kg)
}
if (!exists("nc_real200_k2", envir = .kg, inherits = FALSE)) {
  path <- resolve_data_file("nc_real200_k2.rda")
  if (!is.null(path)) load(path, envir = .kg)
}
stopifnot(exists("nc_real200_k2", envir = .kg, inherits = FALSE))
nc_real200_k2 <- get("nc_real200_k2", envir = .kg, inherits = FALSE)
print_dataset_header(nc_real200_k2, "nc_real200_k2")
#> 
#> Dataset: nc_real200_k2 
#> head(y):
#> [1] -1.7079605 -0.5634694 -2.2334859  1.8734314  1.4309543 -0.3023361
#> meta: n=200 | support=real | p=0 | K_true=2 | tail=FALSE
```


``` r
# Light MCMC settings to keep the vignette fast
mcmc <- list(niter = 300, nburnin = 100, thin = 1, nchains = 2, seed = 1)

bundle_sb <- build_nimble_bundle(
  y = nc_real200_k2$y,
  backend = "sb",
  kernel = "normal",
  GPD = FALSE,
  components = nc_real200_k2$meta$K_true,
  mcmc = mcmc
)

bundle_crp <- build_nimble_bundle(
  y = nc_real200_k2$y,
  backend = "crp",
  kernel = "normal",
  GPD = FALSE,
  components = nc_real200_k2$meta$K_true,
  mcmc = mcmc
)

summary(bundle_sb)
summary(bundle_crp)

fit_sb <- run_mcmc_bundle_manual(bundle_sb, show_progress = FALSE)
fit_crp <- run_mcmc_bundle_manual(bundle_crp, show_progress = FALSE)

summary(fit_sb)
summary(fit_crp)
```
