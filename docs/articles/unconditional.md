# Unconditional Models

## Overview

This vignette demonstrates fitting an unconditional DP mixture model
with optional GPD tail augmentation.

## Model Fitting

``` r
library(DPmixGPD)

n <- 100
y <- abs(rnorm(n)) + 0.2

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
#> categorical sampler (100)
#>   - z[]  (100 elements)
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
#> n = 100 | components = 6 | epsilon = 0.025
#> MCMC: niter=400, nburnin=100, thin=2, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
```

## Fitted Summaries

``` r
f_mean <- fitted(fit, type = "mean", level = 0.90)
head(f_mean)
#>        fit    lower    upper  residuals
#> 1 1.371307 1.002138 1.989462 -0.5448536
#> 2 1.371307 1.002138 1.989462 -0.9876641
#> 3 1.371307 1.002138 1.989462 -0.3356788
#> 4 1.371307 1.002138 1.989462  0.4239734
#> 5 1.371307 1.002138 1.989462 -0.8417996
#> 6 1.371307 1.002138 1.989462 -0.3508390

f_med <- fitted(fit, type = "median", level = 0.90)
head(f_med)
#>         fit     lower    upper   residuals
#> 1 0.7676098 0.7012283 0.857014  0.05884404
#> 2 0.7676098 0.7012283 0.857014 -0.38396645
#> 3 0.7676098 0.7012283 0.857014  0.26801884
#> 4 0.7676098 0.7012283 0.857014  1.02767103
#> 5 0.7676098 0.7012283 0.857014 -0.23810200
#> 6 0.7676098 0.7012283 0.857014  0.25285861
```

## Posterior Predictive Summaries

``` r
pred_mean <- predict(fit, type = "mean", cred.level = 0.90, interval = "credible")
pred_q95  <- predict(fit, type = "quantile", index = 0.95, cred.level = 0.90, interval = "credible")

pred_mean$fit
#>   estimate     lower  upper
#> 1 1.365748 0.9977448 2.0355
pred_q95$fit
#>   estimate index    lower    upper
#> 1 2.056073  0.95 1.788326 2.348427
```

## Residual Analysis

``` r
res <- f_mean$residuals
summary(res)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -1.1702 -0.8392 -0.5774 -0.4546 -0.1224  1.2303
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

![](unconditional_files/figure-html/unnamed-chunk-5-1.png)

    #> 
    #> === density ===

![](unconditional_files/figure-html/unnamed-chunk-5-2.png)

    #> 
    #> === traceplot ===

![](unconditional_files/figure-html/unnamed-chunk-5-3.png)

    #> 
    #> === running ===

![](unconditional_files/figure-html/unnamed-chunk-5-4.png)

    #> 
    #> === compare_partial ===

![](unconditional_files/figure-html/unnamed-chunk-5-5.png)

    #> 
    #> === autocorrelation ===

![](unconditional_files/figure-html/unnamed-chunk-5-6.png)

    #> 
    #> === geweke ===

![](unconditional_files/figure-html/unnamed-chunk-5-7.png)

    #> 
    #> === caterpillar ===

![](unconditional_files/figure-html/unnamed-chunk-5-8.png)

    #> 
    #> === pairs ===

![](unconditional_files/figure-html/unnamed-chunk-5-9.png)
