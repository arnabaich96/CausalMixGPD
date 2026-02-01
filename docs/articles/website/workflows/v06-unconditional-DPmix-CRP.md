# 6. Unconditional DPmix with CRP Backend

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Unconditional DPmix: Chinese Restaurant Process (CRP)

**Goal**: Estimate density of univariate outcome $`y`$ using
**nonparametric Dirichlet Process mixture** with **Chinese Restaurant
Process** backend.

**Model**: $`y_i | G \sim \int K(y_i; \theta) dG(\theta)`$ where
$`G \sim \text{DP}(\alpha, G_0)`$

**Backend**: CRP with **truncation at max components**

------------------------------------------------------------------------

### Data Setup

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
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6, fill = "steelblue",color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Raw Data: Mixed Gamma Distribution", x = "y", y = "Density") +
  theme_minimal()

print(p_raw)
```

![](v06-unconditional-DPmix-CRP_files/figure-html/data-setup-1.png)

------------------------------------------------------------------------

### Model Specification & Bundle

We’ll use the `build_nimble_bundle` function directly which handles both
specification and bundle creation.

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

------------------------------------------------------------------------

#### Building MCMC bundle

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

#### Summary of MCMC Bundle

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

#### Running MCMC

``` r

fit_crp <- load_or_fit("v06-unconditional-DPmix-CRP-fit_crp", run_mcmc_bundle_manual(bundle_crp))
```

#### Summary of Fitted MCMC model

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

------------------------------------------------------------------------

### MCMC Diagnostics Plots

``` r

# Trace plots for key parameters
if (interactive()) plot(fit_crp, params = "alpha", family = c("traceplot", "density", "geweke"))
```

------------------------------------------------------------------------

### Posterior Predictions

#### Predictive Density

``` r

# Generate prediction grid
y_grid <- seq(0, max(y_mixed) * 1.2, length.out = 200)

# Posterior predictive density
pred_density <- predict(fit_crp, y = y_grid, type = "density")

# Use S3 plot method
if (interactive()) plot(pred_density)
```

#### Quantile Predictions

``` r

# Posterior predictive quantiles with credible intervals
quantiles_pred <- predict(fit_crp, type = "quantile", 
                          index = c(0.05, 0.25, 0.5, 0.75, 0.95),
                          interval = "credible")

# Display table
quantiles_pred$fit %>%
 kbl(caption = "Posterior Predictive Quantiles with Credible Intervals",
   align = "c",
                  digits = 3) %>%
 kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
```

| estimate | index | lower | upper |
|:--------:|:-----:|:-----:|:-----:|
|  -0.39   | 0.05  | -2.57 | 1.16  |
|   1.93   | 0.25  | 0.94  | 2.84  |
|   5.90   | 0.50  | 3.05  | 7.14  |
|   7.00   | 0.75  | 5.87  | 8.02  |
|   7.56   | 0.95  | 6.44  | 8.55  |

Posterior Predictive Quantiles with Credible Intervals {.table .table
.table-striped
style="width: auto !important; margin-left: auto; margin-right: auto;"}

``` r

# Use S3 plot method
if (interactive()) plot(quantiles_pred)
```

------------------------------------------------------------------------

### Varying Truncation Level (components)

``` r

# Demonstrate with one value
bundle_components <- build_nimble_bundle(
  y = y_mixed,
  kernel = "laplace",
  backend = "crp",
  components = 5,
  mcmc = mcmc
)
fit_components <- load_or_fit("v06-unconditional-DPmix-CRP-fit_components", run_mcmc_bundle_manual(bundle_components))
```

``` r

summary(fit_components)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Laplace Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 200 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 3

    WAIC: 891.092
    lppd: -295.218 | pWAIC: 150.327

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
       <td style="text-align:center;"> 0.39 </td>
       <td style="text-align:center;"> 0.05 </td>
       <td style="text-align:center;"> 0.28 </td>
       <td style="text-align:center;"> 0.395 </td>
       <td style="text-align:center;"> 0.48 </td>
       <td style="text-align:center;"> 124.026 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> weights[2] </td>
       <td style="text-align:center;"> 0.296 </td>
       <td style="text-align:center;"> 0.05 </td>
       <td style="text-align:center;"> 0.2 </td>
       <td style="text-align:center;"> 0.295 </td>
       <td style="text-align:center;"> 0.39 </td>
       <td style="text-align:center;"> 166.462 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> weights[3] </td>
       <td style="text-align:center;"> 0.213 </td>
       <td style="text-align:center;"> 0.046 </td>
       <td style="text-align:center;"> 0.125 </td>
       <td style="text-align:center;"> 0.21 </td>
       <td style="text-align:center;"> 0.295 </td>
       <td style="text-align:center;"> 204.79 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.752 </td>
       <td style="text-align:center;"> 0.404 </td>
       <td style="text-align:center;"> 0.187 </td>
       <td style="text-align:center;"> 0.678 </td>
       <td style="text-align:center;"> 1.733 </td>
       <td style="text-align:center;"> 541.565 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> location[1] </td>
       <td style="text-align:center;"> 5.06 </td>
       <td style="text-align:center;"> 2.48 </td>
       <td style="text-align:center;"> 0.923 </td>
       <td style="text-align:center;"> 6.499 </td>
       <td style="text-align:center;"> 7.902 </td>
       <td style="text-align:center;"> 68.982 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> location[2] </td>
       <td style="text-align:center;"> 3.143 </td>
       <td style="text-align:center;"> 2.637 </td>
       <td style="text-align:center;"> 0.709 </td>
       <td style="text-align:center;"> 2.287 </td>
       <td style="text-align:center;"> 9.177 </td>
       <td style="text-align:center;"> 248.114 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> location[3] </td>
       <td style="text-align:center;"> 2.572 </td>
       <td style="text-align:center;"> 2.573 </td>
       <td style="text-align:center;"> 0.444 </td>
       <td style="text-align:center;"> 1.502 </td>
       <td style="text-align:center;"> 9.801 </td>
       <td style="text-align:center;"> 104.292 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[1] </td>
       <td style="text-align:center;"> 0.705 </td>
       <td style="text-align:center;"> 0.558 </td>
       <td style="text-align:center;"> 0.263 </td>
       <td style="text-align:center;"> 0.363 </td>
       <td style="text-align:center;"> 2.13 </td>
       <td style="text-align:center;"> 63.482 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[2] </td>
       <td style="text-align:center;"> 1.344 </td>
       <td style="text-align:center;"> 0.73 </td>
       <td style="text-align:center;"> 0.284 </td>
       <td style="text-align:center;"> 1.315 </td>
       <td style="text-align:center;"> 2.798 </td>
       <td style="text-align:center;"> 630.231 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[3] </td>
       <td style="text-align:center;"> 1.853 </td>
       <td style="text-align:center;"> 0.941 </td>
       <td style="text-align:center;"> 0.291 </td>
       <td style="text-align:center;"> 1.833 </td>
       <td style="text-align:center;"> 3.784 </td>
       <td style="text-align:center;"> 218.337 </td>
      </tr>
    </tbody>
    </table>

------------------------------------------------------------------------

For unconditional models, use
[`predict()`](https://rdrr.io/r/stats/predict.html) for posterior
summaries; [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
[`residuals()`](https://rdrr.io/r/stats/residuals.html) are not
supported.

------------------------------------------------------------------------

### Model Comparison: Different Kernels

#### Laplace Kernel (Current)

``` r

bundle_laplace <- build_nimble_bundle(
  y = y_mixed,
  kernel = "laplace",
  backend = "crp",
  components = 5,
  mcmc = mcmc
)
fit_laplace <- load_or_fit("v06-unconditional-DPmix-CRP-fit_laplace", run_mcmc_bundle_manual(bundle_laplace))
```

``` r

summary(fit_laplace)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Laplace Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 200 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 2

    WAIC: 889.569
    lppd: -307.083 | pWAIC: 137.701

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
       <td style="text-align:center;"> 0.392 </td>
       <td style="text-align:center;"> 0.053 </td>
       <td style="text-align:center;"> 0.275 </td>
       <td style="text-align:center;"> 0.395 </td>
       <td style="text-align:center;"> 0.48 </td>
       <td style="text-align:center;"> 151.984 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> weights[2] </td>
       <td style="text-align:center;"> 0.305 </td>
       <td style="text-align:center;"> 0.055 </td>
       <td style="text-align:center;"> 0.205 </td>
       <td style="text-align:center;"> 0.305 </td>
       <td style="text-align:center;"> 0.41 </td>
       <td style="text-align:center;"> 138.525 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.737 </td>
       <td style="text-align:center;"> 0.381 </td>
       <td style="text-align:center;"> 0.154 </td>
       <td style="text-align:center;"> 0.677 </td>
       <td style="text-align:center;"> 1.63 </td>
       <td style="text-align:center;"> 903.061 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> location[1] </td>
       <td style="text-align:center;"> 4.736 </td>
       <td style="text-align:center;"> 2.615 </td>
       <td style="text-align:center;"> 0.909 </td>
       <td style="text-align:center;"> 6.299 </td>
       <td style="text-align:center;"> 7.888 </td>
       <td style="text-align:center;"> 119.679 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> location[2] </td>
       <td style="text-align:center;"> 3.459 </td>
       <td style="text-align:center;"> 2.822 </td>
       <td style="text-align:center;"> 0.718 </td>
       <td style="text-align:center;"> 2.326 </td>
       <td style="text-align:center;"> 9.378 </td>
       <td style="text-align:center;"> 413.229 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[1] </td>
       <td style="text-align:center;"> 0.789 </td>
       <td style="text-align:center;"> 0.586 </td>
       <td style="text-align:center;"> 0.263 </td>
       <td style="text-align:center;"> 0.392 </td>
       <td style="text-align:center;"> 2.135 </td>
       <td style="text-align:center;"> 103.007 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[2] </td>
       <td style="text-align:center;"> 1.265 </td>
       <td style="text-align:center;"> 0.739 </td>
       <td style="text-align:center;"> 0.267 </td>
       <td style="text-align:center;"> 1.262 </td>
       <td style="text-align:center;"> 2.669 </td>
       <td style="text-align:center;"> 415.897 </td>
      </tr>
    </tbody>
    </table>

#### Amoroso Kernel (Alternative)

``` r

bundle_amoroso <- build_nimble_bundle(
  y = y_mixed,
  kernel = "amoroso",
  backend = "crp",
  components = 5,
  mcmc = mcmc
)
fit_amoroso <- load_or_fit("v06-unconditional-DPmix-CRP-fit_amoroso", run_mcmc_bundle_manual(bundle_amoroso))
```

``` r

summary(fit_amoroso)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Amoroso Distribution | GPD tail: FALSE | epsilon: 0.025
    n = 200 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 1

    WAIC: 921.494
    lppd: -418.642 | pWAIC: 42.105

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
       <td style="text-align:center;"> 0.901 </td>
       <td style="text-align:center;"> 0.175 </td>
       <td style="text-align:center;"> 0.495 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 4.591 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.292 </td>
       <td style="text-align:center;"> 0.29 </td>
       <td style="text-align:center;"> 0.007 </td>
       <td style="text-align:center;"> 0.201 </td>
       <td style="text-align:center;"> 1.101 </td>
       <td style="text-align:center;"> 140.654 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> loc[1] </td>
       <td style="text-align:center;"> 0.017 </td>
       <td style="text-align:center;"> 0.32 </td>
       <td style="text-align:center;"> -0.385 </td>
       <td style="text-align:center;"> 0.01 </td>
       <td style="text-align:center;"> 1.264 </td>
       <td style="text-align:center;"> 257.023 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> scale[1] </td>
       <td style="text-align:center;"> 2.333 </td>
       <td style="text-align:center;"> 1.18 </td>
       <td style="text-align:center;"> 0.543 </td>
       <td style="text-align:center;"> 2.213 </td>
       <td style="text-align:center;"> 4.932 </td>
       <td style="text-align:center;"> 14.698 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> shape1[1] </td>
       <td style="text-align:center;"> 1.624 </td>
       <td style="text-align:center;"> 0.617 </td>
       <td style="text-align:center;"> 0.532 </td>
       <td style="text-align:center;"> 1.524 </td>
       <td style="text-align:center;"> 2.98 </td>
       <td style="text-align:center;"> 19.586 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> shape2[1] </td>
       <td style="text-align:center;"> 0.91 </td>
       <td style="text-align:center;"> 0.326 </td>
       <td style="text-align:center;"> 0.566 </td>
       <td style="text-align:center;"> 0.834 </td>
       <td style="text-align:center;"> 1.943 </td>
       <td style="text-align:center;"> 10.792 </td>
      </tr>
    </tbody>
    </table>

#### Model Comparison via Predictions

``` r

# Compare models using predict() (fitted/residuals not supported for unconditional)
pred_laplace <- predict(fit_laplace, type = "mean", cred.level = 0.9, interval = "credible")
pred_amoroso <- predict(fit_amoroso, type = "mean", cred.level = 0.9, interval = "credible")
```

``` r

# For unconditional models, fitted() is not supported; use predict() for comparisons (see pred_laplace, pred_amoroso above).
```

### Key Takeaways

- **CRP Backend**: Flexible component allocation, ideal for unknown
  mixture complexity
- **components Parameter**: Higher components allows more components but
  increases computation
- **Kernel Choice**: Gamma suitable for positive, skewed data
- **Diagnostics**: Check convergence (Rhat, ESS) and posterior
  predictive fit
- **Next**: Compare with **Stick-Breaking (SB)** backend in vignette 5
