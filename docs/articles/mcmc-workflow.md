# MCMC Workflow

## Overview

This vignette demonstrates the MCMC workflow: build, run, inspect, and
extract.

## Theory (brief)

The posterior is explored with Markov chain Monte Carlo. Diagnostics
summarize mixing and convergence behavior, such as traceplots, effective
sample size, and Geweke statistics comparing early vs late chain
segments. These checks help ensure that posterior summaries are
reliable.

## Model Building and Sampling

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

## Model Summary

``` r
print(fit)
MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE
n = 272 | components = 6 | epsilon = 0.025
MCMC: niter=400, nburnin=100, thin=2, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
summary(fit)
MixGPD summary | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE | epsilon: 0.025
n = 272 | components = 6
Summary
Initial components: 6 | Components after truncation: 3

WAIC: 733.039
lppd: -359.414 | pWAIC: 7.106

Summary table
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> parameter </th>
   <th style="text-align:center;"> mean </th>
   <th style="text-align:center;"> sd </th>
   <th style="text-align:center;"> q0.025 </th>
   <th style="text-align:center;"> q0.500 </th>
   <th style="text-align:center;"> q0.975 </th>
   <th style="text-align:center;"> ess </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> weights[1] </td>
   <td style="text-align:center;"> 0.433 </td>
   <td style="text-align:center;"> 0.081 </td>
   <td style="text-align:center;"> 0.323 </td>
   <td style="text-align:center;"> 0.417 </td>
   <td style="text-align:center;"> 0.597 </td>
   <td style="text-align:center;"> 3.046 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> weights[2] </td>
   <td style="text-align:center;"> 0.276 </td>
   <td style="text-align:center;"> 0.056 </td>
   <td style="text-align:center;"> 0.162 </td>
   <td style="text-align:center;"> 0.285 </td>
   <td style="text-align:center;"> 0.371 </td>
   <td style="text-align:center;"> 7.301 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> weights[3] </td>
   <td style="text-align:center;"> 0.192 </td>
   <td style="text-align:center;"> 0.051 </td>
   <td style="text-align:center;"> 0.107 </td>
   <td style="text-align:center;"> 0.197 </td>
   <td style="text-align:center;"> 0.282 </td>
   <td style="text-align:center;"> 4.688 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> alpha </td>
   <td style="text-align:center;"> 1.281 </td>
   <td style="text-align:center;"> 0.659 </td>
   <td style="text-align:center;"> 0.38 </td>
   <td style="text-align:center;"> 1.273 </td>
   <td style="text-align:center;"> 2.92 </td>
   <td style="text-align:center;"> 16.212 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> tail_scale </td>
   <td style="text-align:center;"> 2.57 </td>
   <td style="text-align:center;"> 0.086 </td>
   <td style="text-align:center;"> 2.423 </td>
   <td style="text-align:center;"> 2.561 </td>
   <td style="text-align:center;"> 2.689 </td>
   <td style="text-align:center;"> 5.752 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> tail_shape </td>
   <td style="text-align:center;"> -0.729 </td>
   <td style="text-align:center;"> 0.038 </td>
   <td style="text-align:center;"> -0.769 </td>
   <td style="text-align:center;"> -0.749 </td>
   <td style="text-align:center;"> -0.618 </td>
   <td style="text-align:center;"> 2.31 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> threshold </td>
   <td style="text-align:center;"> 1.687 </td>
   <td style="text-align:center;"> 0.027 </td>
   <td style="text-align:center;"> 1.664 </td>
   <td style="text-align:center;"> 1.664 </td>
   <td style="text-align:center;"> 1.728 </td>
   <td style="text-align:center;"> 2.389 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean[1] </td>
   <td style="text-align:center;"> 7.592 </td>
   <td style="text-align:center;"> 3.263 </td>
   <td style="text-align:center;"> 3.042 </td>
   <td style="text-align:center;"> 7.262 </td>
   <td style="text-align:center;"> 14.606 </td>
   <td style="text-align:center;"> 23.106 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean[2] </td>
   <td style="text-align:center;"> 7.402 </td>
   <td style="text-align:center;"> 2.658 </td>
   <td style="text-align:center;"> 3.605 </td>
   <td style="text-align:center;"> 7.203 </td>
   <td style="text-align:center;"> 12.746 </td>
   <td style="text-align:center;"> 63.863 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> mean[3] </td>
   <td style="text-align:center;"> 6.89 </td>
   <td style="text-align:center;"> 2.362 </td>
   <td style="text-align:center;"> 3.702 </td>
   <td style="text-align:center;"> 6.396 </td>
   <td style="text-align:center;"> 12.253 </td>
   <td style="text-align:center;"> 25.889 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> sd[1] </td>
   <td style="text-align:center;"> 1.782 </td>
   <td style="text-align:center;"> 1.234 </td>
   <td style="text-align:center;"> 0.134 </td>
   <td style="text-align:center;"> 1.418 </td>
   <td style="text-align:center;"> 4.977 </td>
   <td style="text-align:center;"> 5.532 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> sd[2] </td>
   <td style="text-align:center;"> 1.378 </td>
   <td style="text-align:center;"> 0.783 </td>
   <td style="text-align:center;"> 0.353 </td>
   <td style="text-align:center;"> 1.17 </td>
   <td style="text-align:center;"> 3.432 </td>
   <td style="text-align:center;"> 28.357 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> sd[3] </td>
   <td style="text-align:center;"> 1.379 </td>
   <td style="text-align:center;"> 0.842 </td>
   <td style="text-align:center;"> 0.199 </td>
   <td style="text-align:center;"> 1.251 </td>
   <td style="text-align:center;"> 3.217 </td>
   <td style="text-align:center;"> 26.071 </td>
  </tr>
</tbody>
</table>
```

## Diagnostic Plots

``` r

if (requireNamespace("ggmcmc", quietly = TRUE) && requireNamespace("coda", quietly = TRUE)) {
  smp <- fit$mcmc$samples
  params <- if (!is.null(smp)) {
    cn <- colnames(as.matrix(smp))
    cn[1:min(6, length(cn))]
  } else {
    NULL
  }
  if (interactive()) plot(fit, family = "geweke", params = params)
} else {
  message("Plotting requires 'ggmcmc' and 'coda' packages.")
}
```

## Posterior Sample Extraction

``` r
if (!is.null(fit$mcmc$samples)) {
  s <- fit$mcmc$samples
  if (requireNamespace("coda", quietly = TRUE)) {
    mat <- as.matrix(s)
    dim(mat)
    colnames(mat)[1:min(20, ncol(mat))]
  } else {
    message("Sample extraction requires 'coda' package.")
  }
}
 [1] "alpha"      "mean[1]"    "mean[2]"    "mean[3]"    "mean[4]"   
 [6] "mean[5]"    "mean[6]"    "sd[1]"      "sd[2]"      "sd[3]"     
[11] "sd[4]"      "sd[5]"      "sd[6]"      "tail_scale" "tail_shape"
[16] "threshold"  "w[1]"       "w[2]"       "w[3]"       "w[4]"      
```

## Re-running with Different Settings

``` r

# Rebuild with modified MCMC settings
# bundle2 <- update_mcmc(bundle, niter = 8000, nburnin = 2000)
# fit2 <- run_mcmc_bundle_manual(bundle2)
```
