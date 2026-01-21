# DPmixGPD: Quick Start

## Overview

This vignette provides an end-to-end introduction to the DPmixGPD
workflow:

- Build a model specification
- Run MCMC sampling
- Extract fitted values and predictions

## Model Description

DPmixGPD fits flexible mixture models for the bulk of the distribution
and can splice a Generalized Pareto Distribution (GPD) tail beyond a
threshold. This approach is appropriate when:

- The center of the data is not well described by a single parametric
  family
- The extreme right tail requires principled extrapolation

## Minimal Example

``` r
library(DPmixGPD)

n <- 80
y <- abs(rnorm(n)) + 0.15

bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
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
#> categorical sampler (80)
#>   - z[]  (80 elements)
#> [MCMC] MCMC configured.
#> [MCMC] Building MCMC object...
#> [MCMC] MCMC object built.
#> [MCMC] Attempting NIMBLE compilation (this may take a minute)...
#> [MCMC] Compiling model...
#> [MCMC] Compiling MCMC sampler...
#> [MCMC] Compilation successful.
#> [MCMC] MCMC execution complete. Processing results...
fit
#> MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE
#> n = 80 | components = 6 | epsilon = 0.025
#> MCMC: niter=400, nburnin=100, thin=2, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
```

## Fitted Values and Residuals

``` r
f <- fitted(fit, type = "mean", level = 0.90)
head(f)
#>         fit     lower   upper   residuals
#> 1 0.8630258 0.7083039 1.00949 -0.08657197
#> 2 0.8630258 0.7083039 1.00949 -0.52938246
#> 3 0.8630258 0.7083039 1.00949  0.12260283
#> 4 0.8630258 0.7083039 1.00949  0.88225502
#> 5 0.8630258 0.7083039 1.00949 -0.38351801
#> 6 0.8630258 0.7083039 1.00949  0.10744260
summary(f$residuals)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -0.71192 -0.41112 -0.11071 -0.01264  0.20973  1.68859
```

## Predictions

``` r
pred_mean <- predict(fit, type = "mean", cred.level = 0.90, interval = "credible")
pred_q90  <- predict(fit, type = "quantile", index = 0.90, cred.level = 0.90, interval = "credible")

pred_mean$fit
#>    estimate     lower    upper
#> 1 0.8614389 0.7192627 1.001022
pred_q90$fit
#>   estimate index    lower   upper
#> 1 1.620146   0.9 1.228256 1.92826
```

## Diagnostic Plots

``` r
if (requireNamespace("ggmcmc", quietly = TRUE) && requireNamespace("coda", quietly = TRUE)) {
  plot(fit)
} else {
  message("Plotting requires 'ggmcmc' and 'coda' packages.")
}
#> 
#> === histogram ===
```

![](introduction_files/figure-html/plot-fit-1.png)

    #> 
    #> === density ===

![](introduction_files/figure-html/plot-fit-2.png)

    #> 
    #> === traceplot ===

![](introduction_files/figure-html/plot-fit-3.png)

    #> 
    #> === running ===

![](introduction_files/figure-html/plot-fit-4.png)

    #> 
    #> === compare_partial ===

![](introduction_files/figure-html/plot-fit-5.png)

    #> 
    #> === autocorrelation ===

![](introduction_files/figure-html/plot-fit-6.png)

    #> 
    #> === geweke ===

![](introduction_files/figure-html/plot-fit-7.png)

    #> 
    #> === caterpillar ===

![](introduction_files/figure-html/plot-fit-8.png)

    #> 
    #> === pairs ===

![](introduction_files/figure-html/plot-fit-9.png)

## Troubleshooting

- **NIMBLE keyword error**: Rename covariate columns that use reserved
  keywords (e.g., `if` to `x_if`).
- **Disk space error**: Set `TMPDIR`/`TEMP`/`TMP` to a drive with
  sufficient free space.

## Next Steps

- **Model Specification**: Complete documentation of all options
- **Unconditional Models**: Density estimation and tail diagnostics
- **Backends**: Comparison of SB and CRP backends
