---
title: "Backends: SB vs CRP"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Backends: SB vs CRP}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Goal

Compare the two mixture backends:

- **SB**: stick-breaking (finite truncation), controlled by `components`.
- **CRP**: Chinese Restaurant Process (cluster allocations).

We use the same data and kernel, then compare fitted summaries.

## Data


``` r
library(DPmixGPD)

n <- 90
y <- abs(rnorm(n)) + 0.2
```

## Fit SB


``` r
bundle_sb <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit_sb <- run_mcmc_bundle_manual(bundle_sb, show_progress = FALSE)
#> [MCMC] Creating NIMBLE model...
#> [MCMC] NIMBLE model created successfully.
#> [MCMC] Configuring MCMC...
#> ===== Monitors =====
#> thin = 1: alpha, mean, sd, tail_scale, tail_shape, threshold, w, z
#> ===== Samplers =====
#> RW sampler (21)
#>   - alpha
#>   - mean[]  (6 elements)
#>   - sd[]  (6 elements)
#>   - threshold
#>   - tail_scale
#>   - tail_shape
#>   - v[]  (5 elements)
#> categorical sampler (90)
#>   - z[]  (90 elements)
#> [MCMC] MCMC configured.
#> [MCMC] Building MCMC object...
#> [MCMC] MCMC object built.
#> [MCMC] Attempting NIMBLE compilation (this may take a minute)...
#> [MCMC] Compiling model...
#> [MCMC] Compiling MCMC sampler...
#> [MCMC] Compilation successful.
#> [MCMC] MCMC execution complete. Processing results...
```

## Fit CRP


``` r
bundle_crp <- build_nimble_bundle(
  y = y,
  backend = "crp",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit_crp <- run_mcmc_bundle_manual(bundle_crp, show_progress = FALSE)
#> [MCMC] Creating NIMBLE model...
#> [MCMC] NIMBLE model created successfully.
#> [MCMC] Configuring MCMC...
#> ===== Monitors =====
#> thin = 1: alpha, mean, sd, tail_scale, tail_shape, threshold, z
#> ===== Samplers =====
#> RW sampler (3)
#>   - threshold
#>   - tail_scale
#>   - tail_shape
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (12)
#>   - mean[]  (6 elements)
#>   - sd[]  (6 elements)
#> CRP sampler (1)
#>   - z[1:90] 
#> [MCMC] MCMC configured.
#> [MCMC] Building MCMC object...
#> [MCMC] MCMC object built.
#> [MCMC] Attempting NIMBLE compilation (this may take a minute)...
#> [MCMC] Compiling model...
#> [MCMC] Compiling MCMC sampler...
#> [MCMC] Compilation successful.
#> [MCMC] MCMC execution complete. Processing results...
```

## Compare fitted summaries


``` r
mean_sb <- predict(fit_sb, type = "mean", cred.level = 0.90, interval = "credible")$fit
mean_crp <- predict(fit_crp, type = "mean", cred.level = 0.90, interval = "credible")$fit

rbind(
  SB  = unlist(mean_sb[1, c("estimate","lower","upper")]),
  CRP = unlist(mean_crp[1, c("estimate","lower","upper")])
)
#>      estimate     lower    upper
#> SB  0.9128796 0.7736089 1.064666
#> CRP 0.9217923 0.8124476 1.054948
```

## Practical guidance

- Choose **SB** when you want explicit control over truncation (`components`) and stable computation.
- Choose **CRP** when you want the number of clusters to adapt more directly, at the cost of potentially more label/cluster dynamics.

## Troubleshooting

- Compilation errors often point to unsupported kernel/backend combinations or reserved names.
- Start with small `niter` and one chain, then scale up.


