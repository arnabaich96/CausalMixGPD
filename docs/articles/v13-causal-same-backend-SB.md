# 13. Causal Inference: Same Backend (SB) - Laplace Kernel

## Causal Inference: Same Backend (SB) - Laplace Kernel

This vignette fits two SB-based causal models using the same kernel
(Laplace):

- Model A: bulk-only outcome models (GPD = FALSE)
- Model B: GPD-augmented outcome models (GPD = TRUE)

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
  kable(caption = "Causal Dataset Summary", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")
```

| statistic |  value   |
|:---------:|:--------:|
|     N     | 500.0000 |
|   Mean    |  0.2737  |
|    SD     |  1.7640  |
|    Min    | -8.0890  |
|    Max    |  5.2750  |

Causal Dataset Summary

``` r

x_eval <- X[1:40, , drop = FALSE]
y_eval <- y[1:40]
u_threshold <- as.numeric(stats::quantile(y, 0.8, names = FALSE))
```

``` r
df_causal <- data.frame(y = y, T = as.factor(T), x1 = X[, 1], x2 = X[, 2])

p_scatter <- ggplot(df_causal, aes(x = x1, y = y, color = T)) +
  geom_point(alpha = 0.5) +
  labs(title = "Outcome vs X1 by Treatment", x = "X1", y = "y", color = "Treatment") +
  theme_minimal()

p_scatter
```

![](v13-causal-same-backend-SB_files/figure-html/data-scatter-1.png)

------------------------------------------------------------------------

### Model A: SB Bulk-only (Laplace)

``` r
bundle_sb_bulk <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = "laplace",
  backend = "sb",
  PS = "logit",
  GPD = FALSE,
  J = 5,
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1)
)

bundle_sb_bulk
DPmixGPD causal bundle
PS model: disabled 
Outcome (treated): backend = sb | kernel = laplace 
Outcome (control): backend = sb | kernel = laplace 
GPD tail (treated/control): FALSE / FALSE 
components (treated/control): 5 / 5 
Outcome PS included: FALSE 
epsilon (treated/control): 0.025 / 0.025 
n (control) = 232 | n (treated) = 268 
summary(bundle_sb_bulk)
DPmixGPD causal bundle summary
DPmixGPD causal bundle
PS model: disabled 
Outcome (treated): backend = sb | kernel = laplace 
Outcome (control): backend = sb | kernel = laplace 
GPD tail (treated/control): FALSE / FALSE 
components (treated/control): 5 / 5 
Outcome PS included: FALSE 
epsilon (treated/control): 0.025 / 0.025 
n (control) = 232 | n (treated) = 268 
```

``` r
fit_sb_bulk <- run_mcmc_causal(bundle_sb_bulk)
[MCMC] Creating NIMBLE model...
[MCMC] NIMBLE model created successfully.
[MCMC] Configuring MCMC...
===== Monitors =====
thin = 1: alpha, beta_location, scale, w, z
===== Samplers =====
RW sampler (25)
  - alpha
  - beta_location[]  (20 elements)
  - v[]  (4 elements)
conjugate sampler (5)
  - scale[]  (5 elements)
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
thin = 1: alpha, beta_location, scale, w, z
===== Samplers =====
RW sampler (25)
  - alpha
  - beta_location[]  (20 elements)
  - v[]  (4 elements)
conjugate sampler (5)
  - scale[]  (5 elements)
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
summary(fit_sb_bulk)

-- Outcome fits --
[control]
MixGPD fit | backend: Stick-Breaking Process | kernel: Laplace Distribution | GPD tail: FALSE
n = 232 | components = 5 | epsilon = 0.025
MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

[treated]
MixGPD fit | backend: Stick-Breaking Process | kernel: Laplace Distribution | GPD tail: FALSE
n = 268 | components = 5 | epsilon = 0.025
MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
params(fit_sb_bulk)
Posterior mean parameters (causal)

[treated]
Posterior mean parameters

$alpha
[1] 1.603

$w
[1] 0.4399 0.2806

$beta_location
           x1      x2      x3      x4
comp1  0.1917 -0.5671  0.0233 -0.0818
comp2  0.3978  0.0229 -0.2405  0.6775
comp3 -0.3630  0.5358  0.1141  0.5401
comp4  0.6679 -0.2401 -0.4240  0.3320
comp5  0.0328 -0.4646  0.3480  0.4963

$scale
[1] 0.8536 1.0620

[control]
Posterior mean parameters

$alpha
[1] 1.182

$w
[1] 0.4901 0.2894

$beta_location
           x1      x2      x3      x4
comp1 -0.1813 -0.5874 -0.1778  0.2410
comp2 -0.4954 -0.4709 -0.2027  0.3224
comp3 -0.0059  1.2876 -0.3352  1.8041
comp4 -0.2876  0.7373 -0.1071  0.9430
comp5 -0.5776 -0.5180 -0.0029 -0.6054

$scale
[1] 0.9915 1.1690
```

``` r
plot(fit_sb_bulk, family = c("traceplot", "autocorrelation", "running"))

=== treated ===

=== traceplot ===
```

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-bulk-1.png)

    === autocorrelation ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-bulk-2.png)

    === running ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-bulk-3.png)

    === control ===

    === traceplot ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-bulk-4.png)

    === autocorrelation ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-bulk-5.png)

    === running ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-bulk-6.png)

``` r
pred_mean_bulk <- predict(fit_sb_bulk, x = x_eval, type = "mean",
                          interval = "credible", nsim_mean = 100)
plot(pred_mean_bulk)
```

![](v13-causal-same-backend-SB_files/figure-html/predict-mean-sb-bulk-1.png)![](v13-causal-same-backend-SB_files/figure-html/predict-mean-sb-bulk-2.png)

``` r
pred_q_bulk <- predict(fit_sb_bulk, x = x_eval, type = "quantile",
                       p = 0.5, interval = "credible")
plot(pred_q_bulk)
```

![](v13-causal-same-backend-SB_files/figure-html/predict-quantile-sb-bulk-1.png)![](v13-causal-same-backend-SB_files/figure-html/predict-quantile-sb-bulk-2.png)

``` r
pred_d_bulk <- predict(fit_sb_bulk, x = x_eval, y = y_eval,
                       type = "density", interval = "credible")
plot(pred_d_bulk)
```

![](v13-causal-same-backend-SB_files/figure-html/predict-density-sb-bulk-1.png)

``` r
pred_p_bulk <- predict(fit_sb_bulk, x = x_eval, y = y_eval,
                       type = "prob", interval = "credible")
plot(pred_p_bulk)
```

![](v13-causal-same-backend-SB_files/figure-html/predict-prob-sb-bulk-1.png)

``` r
ate_bulk <- ate(fit_sb_bulk, newdata = x_eval,
                interval = "credible", nsim_mean = 100)
plot(ate_bulk)
```

![](v13-causal-same-backend-SB_files/figure-html/ate-sb-bulk-1.png)![](v13-causal-same-backend-SB_files/figure-html/ate-sb-bulk-2.png)

``` r
qte_bulk <- qte(fit_sb_bulk, probs = c(0.25, 0.5, 0.75),
                newdata = x_eval, interval = "credible")
plot(qte_bulk)
```

![](v13-causal-same-backend-SB_files/figure-html/qte-sb-bulk-1.png)![](v13-causal-same-backend-SB_files/figure-html/qte-sb-bulk-2.png)

------------------------------------------------------------------------

### Model B: SB with GPD Tail (Laplace)

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

bundle_sb_gpd <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = "laplace",
  backend = "sb",
  PS = "logit",
  GPD = TRUE,
  J = 5,
  param_specs = param_specs_gpd,
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2)
)

bundle_sb_gpd
DPmixGPD causal bundle
PS model: disabled 
Outcome (treated): backend = sb | kernel = laplace 
Outcome (control): backend = sb | kernel = laplace 
GPD tail (treated/control): TRUE / TRUE 
components (treated/control): 5 / 5 
Outcome PS included: FALSE 
epsilon (treated/control): 0.025 / 0.025 
n (control) = 232 | n (treated) = 268 
summary(bundle_sb_gpd)
DPmixGPD causal bundle summary
DPmixGPD causal bundle
PS model: disabled 
Outcome (treated): backend = sb | kernel = laplace 
Outcome (control): backend = sb | kernel = laplace 
GPD tail (treated/control): TRUE / TRUE 
components (treated/control): 5 / 5 
Outcome PS included: FALSE 
epsilon (treated/control): 0.025 / 0.025 
n (control) = 232 | n (treated) = 268 
```

``` r
fit_sb_gpd <- run_mcmc_causal(bundle_sb_gpd)
[MCMC] Creating NIMBLE model...
[MCMC] NIMBLE model created successfully.
[MCMC] Configuring MCMC...
===== Monitors =====
thin = 1: alpha, beta_location, beta_tail_scale, scale, tail_shape, threshold, w, z
===== Samplers =====
RW sampler (36)
  - alpha
  - scale[]  (5 elements)
  - beta_location[]  (20 elements)
  - threshold
  - beta_tail_scale[]  (4 elements)
  - tail_shape
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
thin = 1: alpha, beta_location, beta_tail_scale, scale, tail_shape, threshold, w, z
===== Samplers =====
RW sampler (36)
  - alpha
  - scale[]  (5 elements)
  - beta_location[]  (20 elements)
  - threshold
  - beta_tail_scale[]  (4 elements)
  - tail_shape
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
summary(fit_sb_gpd)

-- Outcome fits --
[control]
MixGPD fit | backend: Stick-Breaking Process | kernel: Laplace Distribution | GPD tail: TRUE
n = 232 | components = 5 | epsilon = 0.025
MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

[treated]
MixGPD fit | backend: Stick-Breaking Process | kernel: Laplace Distribution | GPD tail: TRUE
n = 268 | components = 5 | epsilon = 0.025
MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
params(fit_sb_gpd)
Posterior mean parameters (causal)

[treated]
Posterior mean parameters

$alpha
[1] 1.716

$w
[1] 0.4824 0.3236

$beta_location
           x1      x2      x3      x4
comp1 -0.0725  0.1078 -0.0001 -0.4822
comp2  0.7871  0.2980  0.1454  2.3559
comp3  0.4721 -0.2300  0.0654  1.7169
comp4  0.2451 -0.3491  0.0336  0.6160
comp5  0.0616  0.3143  0.2781  0.2530

$scale
[1] 1.217 1.343

$beta_tail_scale
[1]  0.03296 -0.14020  0.17380 -0.04924

$tail_shape
[1] 0.02189

[control]
Posterior mean parameters

$alpha
[1] 0.8929

$w
[1] 0.5289 0.3710

$beta_location
           x1      x2      x3      x4
comp1  0.0265 -0.1303 -0.1199 -0.7506
comp2 -0.3324 -0.3868 -0.1051  1.2125
comp3 -0.3905  0.3576 -0.3870  1.3727
comp4  0.0637  0.0750 -0.0416  0.1388
comp5  0.0906  0.1467 -0.2255  0.6238

$scale
[1] 1.10 1.17

$beta_tail_scale
[1]  0.32930  0.16960  0.07451 -0.14080

$tail_shape
[1] -0.02477
```

``` r
plot(fit_sb_gpd, family = c("density", "geweke", "caterpillar"))

=== treated ===

=== density ===
```

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-gpd-1.png)

    === geweke ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-gpd-2.png)

    === caterpillar ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-gpd-3.png)

    === control ===

    === density ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-gpd-4.png)

    === geweke ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-gpd-5.png)

    === caterpillar ===

![](v13-causal-same-backend-SB_files/figure-html/plot-fit-sb-gpd-6.png)

``` r
pred_mean_gpd <- predict(fit_sb_gpd, x = x_eval, type = "mean",
                         interval = "credible", nsim_mean = 100)
plot(pred_mean_gpd)
```

![](v13-causal-same-backend-SB_files/figure-html/predict-mean-sb-gpd-1.png)![](v13-causal-same-backend-SB_files/figure-html/predict-mean-sb-gpd-2.png)

``` r
pred_q_gpd <- predict(fit_sb_gpd, x = x_eval, type = "quantile",
                      p = 0.5, interval = "credible")
plot(pred_q_gpd)
```

![](v13-causal-same-backend-SB_files/figure-html/predict-quantile-sb-gpd-1.png)![](v13-causal-same-backend-SB_files/figure-html/predict-quantile-sb-gpd-2.png)

``` r
pred_d_gpd <- predict(fit_sb_gpd, x = x_eval, y = y_eval,
                      type = "density", interval = "credible")
plot(pred_d_gpd)
```

![](v13-causal-same-backend-SB_files/figure-html/predict-density-sb-gpd-1.png)

``` r
pred_p_gpd <- predict(fit_sb_gpd, x = x_eval, y = y_eval,
                      type = "prob", interval = "credible")
plot(pred_p_gpd)
```

![](v13-causal-same-backend-SB_files/figure-html/predict-prob-sb-gpd-1.png)

``` r
ate_gpd <- ate(fit_sb_gpd, newdata = x_eval,
               interval = "credible", nsim_mean = 100)
plot(ate_gpd)
```

![](v13-causal-same-backend-SB_files/figure-html/ate-sb-gpd-1.png)![](v13-causal-same-backend-SB_files/figure-html/ate-sb-gpd-2.png)

``` r
qte_gpd <- qte(fit_sb_gpd, probs = c(0.25, 0.5, 0.75),
               newdata = x_eval, interval = "credible")
plot(qte_gpd)
```

![](v13-causal-same-backend-SB_files/figure-html/qte-sb-gpd-1.png)![](v13-causal-same-backend-SB_files/figure-html/qte-sb-gpd-2.png)
