# 18. Causal Inference: Mixed Backends (Bulk-only) - Cauchy Kernel

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Causal Inference: Mixed Backends (Bulk-only) - Cauchy Kernel

This vignette fits two mixed-backend causal models with a shared Cauchy
kernel:

- Model A: Treated = SB, Control = CRP
- Model B: Treated = CRP, Control = SB

------------------------------------------------------------------------

### Data Setup

``` r
data("causal_alt_real500_p4_k2")
y <- causal_alt_real500_p4_k2$y
T <- causal_alt_real500_p4_k2$T
X <- as.matrix(causal_alt_real500_p4_k2$X)

summary_tbl <- tibble(
  statistic = c("N", "Mean", "SD", "Min", "Max"),
  value = c(length(y), mean(y), sd(y), min(y), max(y))
)

summary_tbl %>%
  mutate(value = signif(value, 4)) %>%
  kable(caption = "Outcome Summary (Cauchy)", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")
```

| statistic |  value   |
|:---------:|:--------:|
|     N     | 500.0000 |
|   Mean    |  0.2737  |
|    SD     |  1.7640  |
|    Min    | -8.0890  |
|    Max    |  5.2750  |

Outcome Summary (Cauchy)

``` r
x_eval <- X[1:40, , drop = FALSE]
y_eval <- y[1:40]
```

``` r
df_causal <- data.frame(y = y, T = as.factor(T), x1 = X[, 1])

p_scatter <- ggplot(df_causal, aes(x = x1, y = y, color = T)) +
  geom_point(alpha = 0.5) +
  labs(title = "Outcome vs X1", x = "X1", y = "y", color = "Treatment") +
  theme_minimal()

p_scatter
```

![](v18-causal-different-backends-CRP_files/figure-html/data-scatter-1.png)

------------------------------------------------------------------------

### Model A: Treated SB, Control CRP (Cauchy)

``` r
bundle_sb_crp <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = c("cauchy", "cauchy"),
  backend = c("sb", "crp"),
  PS = "logit",
  GPD = c(FALSE, FALSE),
  components = c(5, 5),
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1)
)

bundle_sb_crp
```

    DPmixGPD causal bundle
    PS model: disabled 
    Outcome (treated): backend = sb | kernel = cauchy 
    Outcome (control): backend = crp | kernel = cauchy 
    GPD tail (treated/control): FALSE / FALSE 
    components (treated/control): 5 / 5 
    Outcome PS included: FALSE 
    epsilon (treated/control): 0.025 / 0.025 
    n (control) = 232 | n (treated) = 268 

``` r
summary(bundle_sb_crp)
```

    DPmixGPD causal bundle summary
    DPmixGPD causal bundle
    PS model: disabled 
    Outcome (treated): backend = sb | kernel = cauchy 
    Outcome (control): backend = crp | kernel = cauchy 
    GPD tail (treated/control): FALSE / FALSE 
    components (treated/control): 5 / 5 
    Outcome PS included: FALSE 
    epsilon (treated/control): 0.025 / 0.025 
    n (control) = 232 | n (treated) = 268 

``` r
fit_sb_crp <- run_mcmc_causal(bundle_sb_crp)
```

    [MCMC] Creating NIMBLE model...

    [MCMC] NIMBLE model created successfully.
    [MCMC] Configuring MCMC...
    ===== Monitors =====
    thin = 1: alpha, location, scale, z
    ===== Samplers =====
    CRP_concentration sampler (1)
      - alpha
    CRP_cluster_wrapper sampler (10)
      - location[]  (5 elements)
      - scale[]  (5 elements)
    CRP sampler (1)
      - z[1:232] 
    [MCMC] MCMC configured.
    [MCMC] Building MCMC object...

    [MCMC] MCMC object built.
    [MCMC] Attempting NIMBLE compilation (this may take a minute)...
    [MCMC] Compiling model...

    [MCMC] Compiling MCMC sampler...

    [MCMC] Compilation successful.

    |-------------|-------------|-------------|-------------|
    |  [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
    -------------------------------------------------------|
    [MCMC] MCMC execution complete. Processing results...

    [MCMC] Creating NIMBLE model...

    [MCMC] NIMBLE model created successfully.
    [MCMC] Configuring MCMC...
    ===== Monitors =====
    thin = 1: alpha, beta_location, scale, w, z
    ===== Samplers =====
    RW sampler (30)
      - alpha
      - scale[]  (5 elements)
      - beta_location[]  (20 elements)
      - v[]  (4 elements)
    categorical sampler (268)
      - z[]  (268 elements)
    [MCMC] MCMC configured.
    [MCMC] Building MCMC object...
    [MCMC] MCMC object built.
    [MCMC] Attempting NIMBLE compilation (this may take a minute)...
    [MCMC] Compiling model...

    [MCMC] Compiling MCMC sampler...

    [MCMC] Compilation successful.

    |-------------|-------------|-------------|-------------|
    |-------------------------------------------------------|
    [MCMC] MCMC execution complete. Processing results...

``` r
summary(fit_sb_crp)
```

    -- Outcome fits --
    [control]
    MixGPD fit | backend: Chinese Restaurant Process | kernel: Cauchy Distribution | GPD tail: FALSE
    n = 232 | components = 5 | epsilon = 0.025
    MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
    Fit
    Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

    [treated]
    MixGPD fit | backend: Stick-Breaking Process | kernel: Cauchy Distribution | GPD tail: FALSE
    n = 268 | components = 5 | epsilon = 0.025
    MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
    Fit
    Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

``` r
params(fit_sb_crp)
```

    Posterior mean parameters (causal)

    [treated]
    Posterior mean parameters

    $alpha
    [1] 1.842

    $w
    [1] 0.3642 0.2690 0.1856

    $beta_location
               x1      x2      x3      x4
    comp1  0.1561  0.9044  0.2751  0.4910
    comp2  0.5350 -1.2568 -0.5554  0.3711
    comp3 -0.2288  0.7405 -0.1809 -0.0433
    comp4 -0.2499 -0.2137 -0.4999  0.0471
    comp5  0.1978 -1.6964  1.0204  1.2210

    $scale
    [1] 0.6466 0.5748 0.5353

    [control]
    Posterior mean parameters

    $alpha
    [1] 0.6324

    $w
    [1] 0.4170 0.3015 0.2190

    $location
    [1] -0.04636  0.66170 -0.17040

    $scale
    [1] 0.5236 0.5235 0.4180

``` r
plot(fit_sb_crp, family = c("traceplot", "autocorrelation", "running"))
```

    === treated ===

    === traceplot ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-sb-crp-1.png)

    === autocorrelation ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-sb-crp-2.png)

    === running ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-sb-crp-3.png)

    === control ===

    === traceplot ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-sb-crp-4.png)

    === autocorrelation ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-sb-crp-5.png)

    === running ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-sb-crp-6.png)

``` r
pred_mean_sb_crp <- predict(fit_sb_crp, x = x_eval, type = "mean",
                            interval = "credible", nsim_mean = 100)
plot(pred_mean_sb_crp)
```

![](v18-causal-different-backends-CRP_files/figure-html/predict-mean-sb-crp-1.png)![](v18-causal-different-backends-CRP_files/figure-html/predict-mean-sb-crp-2.png)

``` r
pred_q_sb_crp <- predict(fit_sb_crp, x = x_eval, type = "quantile",
                         p = 0.5, interval = "credible")
plot(pred_q_sb_crp)
```

![](v18-causal-different-backends-CRP_files/figure-html/predict-quantile-sb-crp-1.png)![](v18-causal-different-backends-CRP_files/figure-html/predict-quantile-sb-crp-2.png)

``` r
pred_d_sb_crp <- predict(fit_sb_crp, x = x_eval, y = y_eval,
                         type = "density", interval = "credible")
plot(pred_d_sb_crp)
```

![](v18-causal-different-backends-CRP_files/figure-html/predict-density-sb-crp-1.png)

``` r
pred_surv_sb_crp <- predict(fit_sb_crp, x = x_eval, y = y_eval,
                            type = "survival", interval = "credible")
plot(pred_surv_sb_crp)
```

![](v18-causal-different-backends-CRP_files/figure-html/predict-survival-sb-crp-1.png)

``` r
ate_sb_crp <- ate(fit_sb_crp, newdata = x_eval,
                  interval = "credible", nsim_mean = 100)
plot(ate_sb_crp)
```

![](v18-causal-different-backends-CRP_files/figure-html/ate-sb-crp-1.png)![](v18-causal-different-backends-CRP_files/figure-html/ate-sb-crp-2.png)

``` r
qte_sb_crp <- qte(fit_sb_crp, probs = c(0.25, 0.5, 0.75),
                  newdata = x_eval, interval = "credible")
plot(qte_sb_crp)
```

![](v18-causal-different-backends-CRP_files/figure-html/qte-sb-crp-1.png)![](v18-causal-different-backends-CRP_files/figure-html/qte-sb-crp-2.png)

------------------------------------------------------------------------

### Model B: Treated CRP, Control SB (Cauchy)

``` r
bundle_crp_sb <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = c("cauchy", "cauchy"),
  backend = c("crp", "sb"),
  PS = "logit",
  GPD = c(FALSE, FALSE),
  components = c(5, 5),
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2)
)

bundle_crp_sb
```

    DPmixGPD causal bundle
    PS model: disabled 
    Outcome (treated): backend = crp | kernel = cauchy 
    Outcome (control): backend = sb | kernel = cauchy 
    GPD tail (treated/control): FALSE / FALSE 
    components (treated/control): 5 / 5 
    Outcome PS included: FALSE 
    epsilon (treated/control): 0.025 / 0.025 
    n (control) = 232 | n (treated) = 268 

``` r
summary(bundle_crp_sb)
```

    DPmixGPD causal bundle summary
    DPmixGPD causal bundle
    PS model: disabled 
    Outcome (treated): backend = crp | kernel = cauchy 
    Outcome (control): backend = sb | kernel = cauchy 
    GPD tail (treated/control): FALSE / FALSE 
    components (treated/control): 5 / 5 
    Outcome PS included: FALSE 
    epsilon (treated/control): 0.025 / 0.025 
    n (control) = 232 | n (treated) = 268 

``` r
fit_crp_sb <- run_mcmc_causal(bundle_crp_sb)
```

    [MCMC] Creating NIMBLE model...

    [MCMC] NIMBLE model created successfully.
    [MCMC] Configuring MCMC...
    ===== Monitors =====
    thin = 1: alpha, beta_location, scale, w, z
    ===== Samplers =====
    RW sampler (30)
      - alpha
      - scale[]  (5 elements)
      - beta_location[]  (20 elements)
      - v[]  (4 elements)
    categorical sampler (232)
      - z[]  (232 elements)
    [MCMC] MCMC configured.
    [MCMC] Building MCMC object...
    [MCMC] MCMC object built.
    [MCMC] Attempting NIMBLE compilation (this may take a minute)...
    [MCMC] Compiling model...

    [MCMC] Compiling MCMC sampler...

    [MCMC] Compilation successful.

    |-------------|-------------|-------------|-------------|
    |-------------------------------------------------------|
    [MCMC] MCMC execution complete. Processing results...

    [MCMC] Creating NIMBLE model...

    [MCMC] NIMBLE model created successfully.
    [MCMC] Configuring MCMC...
    ===== Monitors =====
    thin = 1: alpha, location, scale, z
    ===== Samplers =====
    CRP_concentration sampler (1)
      - alpha
    CRP_cluster_wrapper sampler (10)
      - location[]  (5 elements)
      - scale[]  (5 elements)
    CRP sampler (1)
      - z[1:268] 
    [MCMC] MCMC configured.
    [MCMC] Building MCMC object...

    [MCMC] MCMC object built.
    [MCMC] Attempting NIMBLE compilation (this may take a minute)...
    [MCMC] Compiling model...

    [MCMC] Compiling MCMC sampler...

    [MCMC] Compilation successful.

    |-------------|-------------|-------------|-------------|
    |  [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
    -------------------------------------------------------|
    [MCMC] MCMC execution complete. Processing results...

``` r
summary(fit_crp_sb)
```

    -- Outcome fits --
    [control]
    MixGPD fit | backend: Stick-Breaking Process | kernel: Cauchy Distribution | GPD tail: FALSE
    n = 232 | components = 5 | epsilon = 0.025
    MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
    Fit
    Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

    [treated]
    MixGPD fit | backend: Chinese Restaurant Process | kernel: Cauchy Distribution | GPD tail: FALSE
    n = 268 | components = 5 | epsilon = 0.025
    MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
    Fit
    Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

``` r
params(fit_crp_sb)
```

    Posterior mean parameters (causal)

    [treated]
    Posterior mean parameters

    $alpha
    [1] 0.4149

    $w
    [1] 0.5306 0.4452

    $location
    [1]  0.7087 -0.1926

    $scale
    [1] 0.6337 0.5286

    [control]
    Posterior mean parameters

    $alpha
    [1] 1.937

    $w
    [1] 0.3371 0.2508 0.1889

    $beta_location
               x1      x2      x3      x4
    comp1 -0.1033  0.1383 -0.0918 -1.0608
    comp2 -0.1120 -0.2083 -0.2446 -0.1722
    comp3 -0.1308 -0.3945 -0.3063  1.5690
    comp4 -0.3138 -0.1543 -0.1642  0.9584
    comp5 -0.1600 -0.3008  0.0725  0.5797

    $scale
    [1] 0.6275 0.5393 0.4912

``` r
plot(fit_crp_sb, family = c("density", "geweke", "caterpillar"))
```

    === treated ===

    === density ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-crp-sb-1.png)

    === geweke ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-crp-sb-2.png)

    === caterpillar ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-crp-sb-3.png)

    === control ===

    === density ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-crp-sb-4.png)

    === geweke ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-crp-sb-5.png)

    === caterpillar ===

![](v18-causal-different-backends-CRP_files/figure-html/plot-fit-crp-sb-6.png)

``` r
pred_mean_crp_sb <- predict(fit_crp_sb, x = x_eval, type = "mean",
                            interval = "credible", nsim_mean = 100)
plot(pred_mean_crp_sb)
```

![](v18-causal-different-backends-CRP_files/figure-html/predict-mean-crp-sb-1.png)![](v18-causal-different-backends-CRP_files/figure-html/predict-mean-crp-sb-2.png)

``` r
pred_q_crp_sb <- predict(fit_crp_sb, x = x_eval, type = "quantile",
                         p = 0.5, interval = "credible")
plot(pred_q_crp_sb)
```

![](v18-causal-different-backends-CRP_files/figure-html/predict-quantile-crp-sb-1.png)![](v18-causal-different-backends-CRP_files/figure-html/predict-quantile-crp-sb-2.png)

``` r
pred_d_crp_sb <- predict(fit_crp_sb, x = x_eval, y = y_eval,
                         type = "density", interval = "credible")
plot(pred_d_crp_sb)
```

![](v18-causal-different-backends-CRP_files/figure-html/predict-density-crp-sb-1.png)

``` r
pred_surv_crp_sb <- predict(fit_crp_sb, x = x_eval, y = y_eval,
                            type = "survival", interval = "credible")
plot(pred_surv_crp_sb)
```

![](v18-causal-different-backends-CRP_files/figure-html/predict-survival-crp-sb-1.png)

``` r
ate_crp_sb <- ate(fit_crp_sb, newdata = x_eval,
                  interval = "credible", nsim_mean = 100)
plot(ate_crp_sb)
```

![](v18-causal-different-backends-CRP_files/figure-html/ate-crp-sb-1.png)![](v18-causal-different-backends-CRP_files/figure-html/ate-crp-sb-2.png)

``` r
qte_crp_sb <- qte(fit_crp_sb, probs = c(0.25, 0.5, 0.75),
                  newdata = x_eval, interval = "credible")
plot(qte_crp_sb)
```

![](v18-causal-different-backends-CRP_files/figure-html/qte-crp-sb-1.png)![](v18-causal-different-backends-CRP_files/figure-html/qte-crp-sb-2.png)
