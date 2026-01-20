# 14. Causal Inference: No X (CRP) - Gamma Kernel

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Causal Inference: No Covariates (CRP)

This vignette fits **two no-X causal models** where the treatment arms
are modeled independently using **unconditional** outcome models.

- Model A: CRP bulk-only (GPD = FALSE)
- Model B: CRP with GPD tail (GPD = TRUE)

------------------------------------------------------------------------

### Data Setup (No X)

``` r
data("causal_alt_real500_p4_k2")
y <- abs(causal_alt_real500_p4_k2$y) + 0.01
T <- causal_alt_real500_p4_k2$T

summary_tbl <- tibble(
  statistic = c("N", "Mean", "SD", "Min", "Max"),
  value = c(length(y), mean(y), sd(y), min(y), max(y))
)

summary_tbl %>%
  mutate(value = signif(value, 4)) %>%
  print()
```

     [38;5;246m# A tibble: 5 × 2 [39m
      statistic    value
       [3m [38;5;246m<chr> [39m [23m         [3m [38;5;246m<dbl> [39m [23m
     [38;5;250m1 [39m N         500     
     [38;5;250m2 [39m Mean        1.43  
     [38;5;250m3 [39m SD          1.08  
     [38;5;250m4 [39m Min         0.012 [4m6 [24m
     [38;5;250m5 [39m Max         8.10  

``` r
u_threshold <- as.numeric(stats::quantile(y, 0.8, names = FALSE))
y_eval <- y[1:40]
```

------------------------------------------------------------------------

### Model A: CRP Bulk-only (Gamma)

``` r
bundle_crp_bulk <- build_causal_bundle(
  y = y,
  T = T,
  X = NULL,
  kernel = "gamma",
  backend = "crp",
  PS = FALSE,
  GPD = FALSE,
  components = 6,
  mcmc_outcome = list(niter = 300, nburnin = 80, nchains = 1, thin = 1, seed = 1)
)

bundle_crp_bulk
```

    DPmixGPD causal bundle
    PS model: disabled 
    Outcome (treated): backend = crp | kernel = gamma 
    Outcome (control): backend = crp | kernel = gamma 
    GPD tail (treated/control): FALSE / FALSE 
    components (treated/control): 6 / 6 
    Outcome PS included: FALSE 
    epsilon (treated/control): 0.025 / 0.025 
    n (control) = 232 | n (treated) = 268 

``` r
fit_crp_bulk <- quiet_mcmc(run_mcmc_causal(bundle_crp_bulk))
summary(fit_crp_bulk)
```

    -- Outcome fits --
    [control]
    MixGPD fit | backend: Chinese Restaurant Process | kernel: Gamma Distribution | GPD tail: FALSE
    n = 232 | components = 6 | epsilon = 0.025
    MCMC: niter=300, nburnin=80, thin=1, nchains=1 
    Fit
    Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

    [treated]
    MixGPD fit | backend: Chinese Restaurant Process | kernel: Gamma Distribution | GPD tail: FALSE
    n = 268 | components = 6 | epsilon = 0.025
    MCMC: niter=300, nburnin=80, thin=1, nchains=1 
    Fit
    Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

``` r
pred_mean_bulk <- predict(fit_crp_bulk, type = "mean", interval = "credible", nsim_mean = 150)
head(pred_mean_bulk)
```

         ps  estimate     lower      upper
    [1,] NA -1.313784 -1.973323 -0.2823862

``` r
plot(pred_mean_bulk)
```

![](v14-causal-no-x-CRP_files/figure-html/predict-mean-crp-bulk-1.png)![](v14-causal-no-x-CRP_files/figure-html/predict-mean-crp-bulk-2.png)

``` r
pred_q_bulk <- predict(fit_crp_bulk, type = "quantile", p = 0.5, interval = "credible")
head(pred_q_bulk)
```

         ps  estimate     lower      upper
    [1,] NA -1.139858 -1.687581 -0.4101725

``` r
plot(pred_q_bulk)
```

![](v14-causal-no-x-CRP_files/figure-html/predict-quantile-crp-bulk-1.png)![](v14-causal-no-x-CRP_files/figure-html/predict-quantile-crp-bulk-2.png)

``` r
pred_d_bulk <- predict(fit_crp_bulk, y = y_eval, type = "density", interval = "credible")
head(pred_d_bulk)
```

              y ps trt_estimate  trt_lower trt_upper con_estimate  con_lower
    1 0.9001906 NA            1 0.02409229 0.4021935            1 0.03031288
    2 1.3517565 NA            1 0.04349703 0.3423511            1 0.04227198
    3 1.1475287 NA            1 0.03459086 0.3730424            1 0.03643608
    4 1.9323578 NA            1 0.06783753 0.2623051            1 0.05824336
    5 3.3439817 NA            1 0.09321880 0.1785609            1 0.07555778
    6 0.9493979 NA            1 0.02613220 0.3974834            1 0.03142415
      con_upper
    1 0.1806315
    2 0.1636283
    3 0.1668065
    4 0.1638685
    5 0.1621003
    6 0.1777212

``` r
plot(pred_d_bulk)
```

![](v14-causal-no-x-CRP_files/figure-html/predict-density-crp-bulk-1.png)

``` r
pred_surv_bulk <- predict(fit_crp_bulk, y = y_eval, type = "survival", interval = "credible")
head(pred_surv_bulk)
```

              y ps trt_estimate trt_lower trt_upper con_estimate con_lower
    1 0.9001906 NA            1 0.6902667 0.9917456            1 0.7637875
    2 1.3517565 NA            1 0.5210217 0.9765268            1 0.6866502
    3 1.1475287 NA            1 0.5941341 0.9844997            1 0.7193641
    4 1.9323578 NA            1 0.3492719 0.9440786            1 0.6198187
    5 3.3439817 NA            1 0.1197759 0.8162919            1 0.4667351
    6 0.9493979 NA            1 0.6705890 0.9905101            1 0.7545498
      con_upper
    1 0.9881603
    2 0.9690829
    3 0.9789502
    4 0.9303311
    5 0.8168910
    6 0.9865656

``` r
plot(pred_surv_bulk)
```

![](v14-causal-no-x-CRP_files/figure-html/predict-survival-crp-bulk-1.png)

``` r
ate_bulk <- ate(fit_crp_bulk, interval = "credible", nsim_mean = 150)
print(ate_bulk)
```

    ATE (Average Treatment Effect)
      Prediction points: 1
      Conditional (covariates): NO
      Propensity score used: NO
      Posterior mean draws: 150
      Credible interval: credible (95%)

    ATE estimates (treated - control):
     id estimate lower upper
      1   -1.333 -5.16 2.468

``` r
summary(ate_bulk)
```

    ATE Summary
    ================================================== 
    Prediction points: 1
    Conditional: NO | PS used: NO
    Posterior mean draws: 150
    Interval: credible (95%)

    Model specification:
      Backend (trt/con): crp / crp
      Kernel (trt/con): gamma / gamma
      GPD tail (trt/con): NO / NO

    ATE statistics:
      Mean: -1.333 | Median: -1.333
      Range: [-1.333, -1.333]

    Credible interval width:
      Mean: 7.628 | Median: 7.628
      Range: [7.628, 7.628]

``` r
ate_plots_bulk <- plot(ate_bulk)
ate_plots_bulk$treatment_effect
```

![](v14-causal-no-x-CRP_files/figure-html/unnamed-chunk-1-1.png)

``` r
qte_bulk <- qte(fit_crp_bulk, probs = c(0.25, 0.5, 0.75), interval = "credible")
print(qte_bulk)
```

    QTE (Quantile Treatment Effect)
      Prediction points: 1
      Quantile grid: 0.25, 0.5, 0.75
      Conditional (covariates): NO
      Propensity score used: NO
      Credible interval: credible (95%)

    QTE estimates (treated - control):
     index id estimate  lower upper
      0.25  1   -0.470 -2.932 2.316
      0.50  1   -1.140 -4.810 2.712
      0.75  1   -1.904 -7.040 2.830

``` r
summary(qte_bulk)
```

    QTE Summary
    ================================================== 
    Prediction points: 1 | Quantiles: 3
    Quantile grid: 0.25, 0.5, 0.75
    Conditional: NO | PS used: NO
    Interval: credible (95%)

    Model specification:
      Backend (trt/con): crp / crp
      Kernel (trt/con): gamma / gamma
      GPD tail (trt/con): NO / NO

    QTE by quantile:
     quantile mean_qte median_qte min_qte max_qte sd_qte
         0.25   -0.470     -0.470  -0.470  -0.470     NA
         0.50   -1.140     -1.140  -1.140  -1.140     NA
         0.75   -1.904     -1.904  -1.904  -1.904     NA

    Credible interval width:
      Mean: 7.546 | Median: 7.521
      Range: [5.248, 9.87]

``` r
qte_plots_bulk <- plot(qte_bulk)
qte_plots_bulk$treatment_effect
```

![](v14-causal-no-x-CRP_files/figure-html/unnamed-chunk-2-1.png)

------------------------------------------------------------------------

### Model B: CRP with GPD Tail (Gamma)

``` r
param_specs_gpd <- list(
  gpd = list(
    threshold = list(
      mode = "dist",
      dist = "lognormal",
      args = list(meanlog = log(max(u_threshold, .Machine$double.eps)), sdlog = 0.25)
    )
  )
)

bundle_crp_gpd <- tryCatch({
  build_causal_bundle(
    y = y,
    T = T,
    X = NULL,
    kernel = "gamma",
    backend = "crp",
    PS = FALSE,
    GPD = TRUE,
    components = 6,
    param_specs = param_specs_gpd,
    mcmc_outcome = list(niter = 300, nburnin = 80, nchains = 1, thin = 1, seed = 2)
  )
}, error = function(e) {
  message("Skipping CRP + GPD example: ", conditionMessage(e))
  NULL
})

supports_crp_gpd <- !is.null(bundle_crp_gpd)
if (supports_crp_gpd) bundle_crp_gpd
```

``` r
if (isTRUE(supports_crp_gpd)) {
  fit_crp_gpd <- quiet_mcmc(run_mcmc_causal(bundle_crp_gpd))
  summary(fit_crp_gpd)
}
```

``` r
if (isTRUE(supports_crp_gpd)) {
  pred_mean_gpd <- predict(fit_crp_gpd, type = "mean", interval = "credible", nsim_mean = 150)
  head(pred_mean_gpd)
  plot(pred_mean_gpd)
}
```

``` r
if (isTRUE(supports_crp_gpd)) {
  pred_q_gpd <- predict(fit_crp_gpd, type = "quantile", p = 0.5, interval = "credible")
  head(pred_q_gpd)
  plot(pred_q_gpd)
}
```

``` r
if (isTRUE(supports_crp_gpd)) {
  pred_d_gpd <- predict(fit_crp_gpd, y = y_eval, type = "density", interval = "credible")
  head(pred_d_gpd)
  plot(pred_d_gpd)
}
```

``` r
if (isTRUE(supports_crp_gpd)) {
  pred_surv_gpd <- predict(fit_crp_gpd, y = y_eval, type = "survival", interval = "credible")
  head(pred_surv_gpd)
  plot(pred_surv_gpd)
}
```

``` r
if (isTRUE(supports_crp_gpd)) {
  ate_gpd <- ate(fit_crp_gpd, interval = "credible", nsim_mean = 150)
  print(ate_gpd)
  summary(ate_gpd)
}
```

``` r
if (isTRUE(supports_crp_gpd)) {
  ate_plots_gpd <- plot(ate_gpd)
  ate_plots_gpd$treatment_effect
}
```

``` r
if (isTRUE(supports_crp_gpd)) {
  qte_gpd <- qte(fit_crp_gpd, probs = c(0.25, 0.5, 0.75), interval = "credible")
  print(qte_gpd)
  summary(qte_gpd)
}
```

``` r
if (isTRUE(supports_crp_gpd)) {
  qte_plots_gpd <- plot(qte_gpd)
  qte_plots_gpd$treatment_effect
}
```
