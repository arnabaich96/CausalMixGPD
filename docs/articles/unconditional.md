# Unconditional Models

## Overview

This vignette demonstrates fitting an unconditional DP mixture model
with optional GPD tail augmentation.

## Theory (brief)

Unconditional models treat observations as exchangeable draws from a
mixture distribution. The DP prior on the mixing measure $`G`$ yields
flexible density estimation: \$\$ y_i \\mid G \\sim \\int K(y_i;
\\theta)\\, dG(\\theta), \\quad G \\sim \\mathrm{DP}(\\alpha, G_0). \$\$
When \$\\mathrm{GPD} = \\mathrm{TRUE}\$, a GPD tail replaces the bulk
kernel beyond a threshold $`u`$ to stabilize tail behavior.

## Model Fitting

``` r
library(DPmixGPD)

data("faithful", package = "datasets")
y <- faithful$eruptions

bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
```

## Fitted Summaries

``` r
f_mean <- fitted(fit, type = "mean", level = 0.90)
head(f_mean)
#>   fit lower upper residuals
#> 1 3.2  3.06  3.36     0.400
#> 2 3.2  3.06  3.36    -1.400
#> 3 3.2  3.06  3.36     0.133
#> 4 3.2  3.06  3.36    -0.917
#> 5 3.2  3.06  3.36     1.333
#> 6 3.2  3.06  3.36    -0.317

f_med <- fitted(fit, type = "median", level = 0.90)
head(f_med)
#>    fit lower upper residuals
#> 1 3.08     3  3.16     0.524
#> 2 3.08     3  3.16    -1.276
#> 3 3.08     3  3.16     0.257
#> 4 3.08     3  3.16    -0.793
#> 5 3.08     3  3.16     1.457
#> 6 3.08     3  3.16    -0.193
```

## Posterior Predictive Summaries

``` r
pred_mean <- predict(fit, type = "mean", cred.level = 0.90, interval = "credible")
pred_q95  <- predict(fit, type = "quantile", index = 0.95, cred.level = 0.90, interval = "credible")

pred_mean$fit
#>   estimate lower upper
#> 1     3.19  3.05  3.32
pred_q95$fit
#>   estimate index lower upper
#> 1     4.81  0.95  4.72  4.93
```

## Residual Analysis

``` r
res <- f_mean$residuals
summary(res)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  -1.600  -1.038   0.800   0.287   1.254   1.900
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
