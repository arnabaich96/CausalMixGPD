# Backends: SB vs CRP

## Overview

This vignette compares the two mixture backends:

- **SB**: Stick-breaking (finite truncation), controlled by `components`
- **CRP**: Chinese Restaurant Process (cluster allocations)

## Data

``` r
library(DPmixGPD)

n <- 90
y <- abs(rnorm(n)) + 0.2
```

## Stick-Breaking Backend

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

## CRP Backend

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

## Comparison of Fitted Summaries

``` r
mean_sb <- predict(fit_sb, type = "mean", cred.level = 0.90, interval = "credible")$fit
mean_crp <- predict(fit_crp, type = "mean", cred.level = 0.90, interval = "credible")$fit

comparison_df <- data.frame(
  Backend = c("SB", "CRP"),
  Estimate = c(mean_sb$estimate[1], mean_crp$estimate[1]),
  Lower = c(mean_sb$lower[1], mean_crp$lower[1]),
  Upper = c(mean_sb$upper[1], mean_crp$upper[1])
)

kable(comparison_df, digits = 3, align = "c",
      caption = "Posterior Mean Comparison: SB vs CRP") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE, position = "center")
```

| Backend | Estimate | Lower | Upper |
|:-------:|:--------:|:-----:|:-----:|
|   SB    |  0.913   | 0.774 | 1.065 |
|   CRP   |  0.922   | 0.812 | 1.055 |

Posterior Mean Comparison: SB vs CRP

## Backend Selection Guidelines

| Backend | Use Case                                                   |
|---------|------------------------------------------------------------|
| **SB**  | Explicit control over truncation level; stable computation |
| **CRP** | Adaptive cluster number; more label/cluster dynamics       |
