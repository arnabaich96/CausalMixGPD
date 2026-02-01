# 5. Unconditional DPmix (CRP Backend)

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Overview

This vignette demonstrates an **unconditional DPmix model** using the
**CRP backend** on a positive dataset. We use a Laplace kernel for the
bulk distribution and no GPD tail augmentation.

## Data Setup

``` r

# Load pre-generated dataset: 200 observations from mixture of 3 gamma components
data(nc_pos200_k3)
y_mixed <- nc_pos200_k3$y

paste("Sample size:", length(y_mixed))
```

    [1] "Sample size: 200"

``` r

paste("Mean:", mean(y_mixed))
```

    [1] "Mean: 4.21476750434594"

``` r

paste("SD:", sd(y_mixed))
```

    [1] "SD: 4.10835046697183"

``` r

paste("Range:", paste(range(y_mixed), collapse = " to "))
```

    [1] "Range: 0.0403111680208858 to 19.6013451514889"

``` r

# Visualization
df_data <- data.frame(y = y_mixed)
p_raw <- ggplot(df_data, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6,
                 fill = "steelblue", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Raw Data: Mixed Gamma Distribution", x = "y", y = "Density") +
  theme_minimal()

print(p_raw)
```

![](v05-unconditional-DPmix-CRP_files/figure-html/data-setup-1.png)

## Build Bundle (CRP)

``` r

bundle_crp <- build_nimble_bundle(
  y = y_mixed,
  kernel = "laplace",         # Use laplace kernel
  backend = "crp",            # CRP backend
  GPD = FALSE,                # No tail augmentation
  components = 3,             # Minimal for testing
  alpha_random = TRUE,        # Random DP concentration
  mcmc = mcmc
)
```

## Run MCMC (Longer Run)

``` r

bundle_crp <- build_nimble_bundle(
  y_mixed,
  kernel = "laplace",
  backend = "crp",
  GPD = FALSE,
  components = 3,
  alpha_random = TRUE,
  mcmc = mcmc
)
```

``` r

summary(bundle_crp)
```

    DPmixGPD bundle summary
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:center;"> Field </th>
       <th style="text-align:center;"> Value </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:center;"> Backend </td>
       <td style="text-align:center;"> Chinese Restaurant Process </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Kernel </td>
       <td style="text-align:center;"> Laplace Distribution </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Components </td>
       <td style="text-align:center;"> 3 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> N </td>
       <td style="text-align:center;"> 200 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> X </td>
       <td style="text-align:center;"> NO </td>
      </tr>
      <tr>
       <td style="text-align:center;"> GPD </td>
       <td style="text-align:center;"> FALSE </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Epsilon </td>
       <td style="text-align:center;"> 0.025 </td>
      </tr>
    </tbody>
    </table>
    Parameter specification
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:center;"> block </th>
       <th style="text-align:center;"> parameter </th>
       <th style="text-align:center;"> mode </th>
       <th style="text-align:center;"> level </th>
       <th style="text-align:center;"> prior </th>
       <th style="text-align:center;"> link </th>
       <th style="text-align:center;"> notes </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:center;"> meta </td>
       <td style="text-align:center;"> backend </td>
       <td style="text-align:center;"> info </td>
       <td style="text-align:center;"> model </td>
       <td style="text-align:center;"> crp </td>
       <td style="text-align:center;">  </td>
       <td style="text-align:center;">  </td>
      </tr>
      <tr>
       <td style="text-align:center;"> meta </td>
       <td style="text-align:center;"> kernel </td>
       <td style="text-align:center;"> info </td>
       <td style="text-align:center;"> model </td>
       <td style="text-align:center;"> laplace </td>
       <td style="text-align:center;">  </td>
       <td style="text-align:center;">  </td>
      </tr>
      <tr>
       <td style="text-align:center;"> meta </td>
       <td style="text-align:center;"> components </td>
       <td style="text-align:center;"> info </td>
       <td style="text-align:center;"> model </td>
       <td style="text-align:center;"> 3 </td>
       <td style="text-align:center;">  </td>
       <td style="text-align:center;">  </td>
      </tr>
      <tr>
       <td style="text-align:center;"> meta </td>
       <td style="text-align:center;"> N </td>
       <td style="text-align:center;"> info </td>
       <td style="text-align:center;"> model </td>
       <td style="text-align:center;"> 200 </td>
       <td style="text-align:center;">  </td>
       <td style="text-align:center;">  </td>
      </tr>
      <tr>
       <td style="text-align:center;"> meta </td>
       <td style="text-align:center;"> P </td>
       <td style="text-align:center;"> info </td>
       <td style="text-align:center;"> model </td>
       <td style="text-align:center;"> 0 </td>
       <td style="text-align:center;">  </td>
       <td style="text-align:center;">  </td>
      </tr>
      <tr>
       <td style="text-align:center;"> concentration </td>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> dist </td>
       <td style="text-align:center;"> scalar </td>
       <td style="text-align:center;"> gamma(shape=1, rate=1) </td>
       <td style="text-align:center;">  </td>
       <td style="text-align:center;"> stochastic concentration </td>
      </tr>
      <tr>
       <td style="text-align:center;"> bulk </td>
       <td style="text-align:center;"> location </td>
       <td style="text-align:center;"> dist </td>
       <td style="text-align:center;"> component (1:3) </td>
       <td style="text-align:center;"> normal(mean=0, sd=5) </td>
       <td style="text-align:center;">  </td>
       <td style="text-align:center;"> iid across components </td>
      </tr>
      <tr>
       <td style="text-align:center;"> bulk </td>
       <td style="text-align:center;"> scale </td>
       <td style="text-align:center;"> dist </td>
       <td style="text-align:center;"> component (1:3) </td>
       <td style="text-align:center;"> gamma(shape=2, rate=1) </td>
       <td style="text-align:center;">  </td>
       <td style="text-align:center;"> iid across components </td>
      </tr>
    </tbody>
    </table>
    Monitors
      n = 4 
      alpha, z[1:200], location[1:3], scale[1:3]

``` r

fit_crp <- load_or_fit("v05-unconditional-DPmix-CRP-fit_crp", run_mcmc_bundle_manual(bundle_crp))
```

``` r

summary(fit_crp)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Laplace Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 200 | components = 3
    Summary
    Initial components: 3 | Components after truncation: 2

    WAIC: 914.162
    lppd: -346.856 | pWAIC: 110.226

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
       <td style="text-align:center;"> 0.42 </td>
       <td style="text-align:center;"> 0.036 </td>
       <td style="text-align:center;"> 0.36 </td>
       <td style="text-align:center;"> 0.415 </td>
       <td style="text-align:center;"> 0.5 </td>
       <td style="text-align:center;"> 372.213 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> weights[2] </td>
       <td style="text-align:center;"> 0.348 </td>
       <td style="text-align:center;"> 0.036 </td>
       <td style="text-align:center;"> 0.28 </td>
       <td style="text-align:center;"> 0.345 </td>
       <td style="text-align:center;"> 0.415 </td>
       <td style="text-align:center;"> 386.238 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.482 </td>
       <td style="text-align:center;"> 0.296 </td>
       <td style="text-align:center;"> 0.098 </td>
       <td style="text-align:center;"> 0.42 </td>
       <td style="text-align:center;"> 1.197 </td>
       <td style="text-align:center;"> 1200 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> location[1] </td>
       <td style="text-align:center;"> 5.585 </td>
       <td style="text-align:center;"> 2.267 </td>
       <td style="text-align:center;"> 1.052 </td>
       <td style="text-align:center;"> 6.739 </td>
       <td style="text-align:center;"> 7.715 </td>
       <td style="text-align:center;"> 415.444 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> location[2] </td>
       <td style="text-align:center;"> 3.17 </td>
       <td style="text-align:center;"> 2.504 </td>
       <td style="text-align:center;"> 0.808 </td>
       <td style="text-align:center;"> 2.257 </td>
       <td style="text-align:center;"> 7.968 </td>
       <td style="text-align:center;"> 424.174 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[1] </td>
       <td style="text-align:center;"> 0.555 </td>
       <td style="text-align:center;"> 0.42 </td>
       <td style="text-align:center;"> 0.261 </td>
       <td style="text-align:center;"> 0.339 </td>
       <td style="text-align:center;"> 1.708 </td>
       <td style="text-align:center;"> 397.138 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[2] </td>
       <td style="text-align:center;"> 1.202 </td>
       <td style="text-align:center;"> 0.641 </td>
       <td style="text-align:center;"> 0.28 </td>
       <td style="text-align:center;"> 1.202 </td>
       <td style="text-align:center;"> 2.419 </td>
       <td style="text-align:center;"> 480.74 </td>
      </tr>
    </tbody>
    </table>

## Posterior Parameters

``` r

params_crp <- params(fit_crp)
params_crp
```

    Posterior mean parameters

    $alpha
    [1] "0.482"

    $w
    [1] "0.42"  "0.348"

    $location
    [1] "5.585" "3.17" 

    $scale
    [1] "0.555" "1.202"

## Diagnostics

``` r

# Trace plots for key parameters
if (interactive()) plot(fit_crp, params = "alpha", family = c("traceplot", "density", "geweke"))
```

## Posterior Predictive Density

``` r

# Generate prediction grid
y_grid <- seq(0, max(y_mixed) * 1.2, length.out = 200)

# Posterior predictive density
pred_density <- predict(fit_crp, y = y_grid, type = "density")

# Use S3 plot method
if (interactive()) plot(pred_density)
```
