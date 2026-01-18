# 12. Causal Inference: Same Backend (CRP) - Normal Kernel

## Causal Inference: Same Backend (CRP) - Normal Kernel

This vignette fits two CRP-based causal models using the same kernel
(normal):

- Model A: bulk-only outcome models (GPD = FALSE)
- Model B: GPD-augmented outcome models (GPD = TRUE)

Each model demonstrates the full causal S3 workflow: bundle, fit,
predict, plot, ATE, and QTE.

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

![](v12-causal-same-backend-CRP_files/figure-html/data-scatter-1.png)

------------------------------------------------------------------------

### Model A: CRP Bulk-only (Normal)

``` r
bundle_crp_bulk <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = "normal",
  backend = "crp",
  PS = "logit",
  GPD = FALSE,
  J = 5,
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1)
)

bundle_crp_bulk
DPmixGPD causal bundle
PS model: disabled 
Outcome (treated): backend = crp | kernel = normal 
Outcome (control): backend = crp | kernel = normal 
GPD tail (treated/control): FALSE / FALSE 
components (treated/control): 5 / 5 
Outcome PS included: FALSE 
epsilon (treated/control): 0.025 / 0.025 
n (control) = 232 | n (treated) = 268 
summary(bundle_crp_bulk)
DPmixGPD causal bundle summary
DPmixGPD causal bundle
PS model: disabled 
Outcome (treated): backend = crp | kernel = normal 
Outcome (control): backend = crp | kernel = normal 
GPD tail (treated/control): FALSE / FALSE 
components (treated/control): 5 / 5 
Outcome PS included: FALSE 
epsilon (treated/control): 0.025 / 0.025 
n (control) = 232 | n (treated) = 268 
```

``` r
fit_crp_bulk <- run_mcmc_causal(bundle_crp_bulk)
[MCMC] Creating NIMBLE model...
[MCMC] NIMBLE model created successfully.
[MCMC] Configuring MCMC...
===== Monitors =====
thin = 1: alpha, mean, sd, z
===== Samplers =====
CRP_concentration sampler (1)
  - alpha
CRP_cluster_wrapper sampler (10)
  - sd[]  (5 elements)
  - mean[]  (5 elements)
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
thin = 1: alpha, mean, sd, z
===== Samplers =====
CRP_concentration sampler (1)
  - alpha
CRP_cluster_wrapper sampler (10)
  - sd[]  (5 elements)
  - mean[]  (5 elements)
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
summary(fit_crp_bulk)

-- Outcome fits --
[control]
MixGPD fit | backend: Chinese Restaurant Process | kernel: Normal Distribution | GPD tail: FALSE
n = 232 | components = 5 | epsilon = 0.025
MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

[treated]
MixGPD fit | backend: Chinese Restaurant Process | kernel: Normal Distribution | GPD tail: FALSE
n = 268 | components = 5 | epsilon = 0.025
MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
params(fit_crp_bulk)
Posterior mean parameters (causal)

[treated]
Posterior mean parameters

$alpha
[1] 0.6784

$w
[1] 0.5919

$mean
[1] 0.4618

$sd
[1] 0.9184

[control]
Posterior mean parameters

$alpha
[1] 0.5696

$w
[1] 0.5739 0.3219

$mean
[1] -0.3227  1.2000

$sd
[1] 1.042 1.294
```

``` r
plot(fit_crp_bulk, family = c("traceplot", "autocorrelation", "running"))

=== treated ===

=== traceplot ===
```

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-bulk-1.png)

    === autocorrelation ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-bulk-2.png)

    === running ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-bulk-3.png)

    === control ===

    === traceplot ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-bulk-4.png)

    === autocorrelation ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-bulk-5.png)

    === running ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-bulk-6.png)

``` r
pred_mean_bulk <- predict(fit_crp_bulk, x = x_eval, type = "mean",
                          interval = "credible", nsim_mean = 100)
plot(pred_mean_bulk)
```

![](v12-causal-same-backend-CRP_files/figure-html/predict-mean-crp-bulk-1.png)![](v12-causal-same-backend-CRP_files/figure-html/predict-mean-crp-bulk-2.png)

``` r
pred_q_bulk <- predict(fit_crp_bulk, x = x_eval, type = "quantile",
                       p = 0.5, interval = "credible")
plot(pred_q_bulk)
```

![](v12-causal-same-backend-CRP_files/figure-html/predict-quantile-crp-bulk-1.png)![](v12-causal-same-backend-CRP_files/figure-html/predict-quantile-crp-bulk-2.png)

``` r
pred_d_bulk <- predict(fit_crp_bulk, x = x_eval, y = y_eval,
                       type = "density", interval = "credible")
plot(pred_d_bulk)
```

![](v12-causal-same-backend-CRP_files/figure-html/predict-density-crp-bulk-1.png)

``` r
pred_p_bulk <- predict(fit_crp_bulk, x = x_eval, y = y_eval,
                       type = "prob", interval = "credible")
plot(pred_p_bulk)
```

![](v12-causal-same-backend-CRP_files/figure-html/predict-prob-crp-bulk-1.png)

``` r
ate_bulk <- ate(fit_crp_bulk, newdata = x_eval,
                interval = "credible", nsim_mean = 100)
plot(ate_bulk)
```

![](v12-causal-same-backend-CRP_files/figure-html/ate-crp-bulk-1.png)![](v12-causal-same-backend-CRP_files/figure-html/ate-crp-bulk-2.png)

``` r
qte_bulk <- qte(fit_crp_bulk, probs = c(0.25, 0.5, 0.75),
                newdata = x_eval, interval = "credible")
plot(qte_bulk)
```

![](v12-causal-same-backend-CRP_files/figure-html/qte-crp-bulk-1.png)![](v12-causal-same-backend-CRP_files/figure-html/qte-crp-bulk-2.png)

------------------------------------------------------------------------

### Model B: CRP with GPD Tail (Normal)

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

bundle_crp_gpd <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = "normal",
  backend = "crp",
  PS = "logit",
  GPD = TRUE,
  J = 5,
  param_specs = param_specs_gpd,
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2)
)

bundle_crp_gpd
DPmixGPD causal bundle
PS model: disabled 
Outcome (treated): backend = crp | kernel = normal 
Outcome (control): backend = crp | kernel = normal 
GPD tail (treated/control): TRUE / TRUE 
components (treated/control): 5 / 5 
Outcome PS included: FALSE 
epsilon (treated/control): 0.025 / 0.025 
n (control) = 232 | n (treated) = 268 
summary(bundle_crp_gpd)
DPmixGPD causal bundle summary
DPmixGPD causal bundle
PS model: disabled 
Outcome (treated): backend = crp | kernel = normal 
Outcome (control): backend = crp | kernel = normal 
GPD tail (treated/control): TRUE / TRUE 
components (treated/control): 5 / 5 
Outcome PS included: FALSE 
epsilon (treated/control): 0.025 / 0.025 
n (control) = 232 | n (treated) = 268 
```

``` r
fit_crp_gpd <- run_mcmc_causal(bundle_crp_gpd)
[MCMC] Creating NIMBLE model...
[MCMC] NIMBLE model created successfully.
[MCMC] Configuring MCMC...
===== Monitors =====
thin = 1: alpha, beta_tail_scale, mean, sd, tail_shape, threshold, z
===== Samplers =====
RW sampler (6)
  - threshold
  - beta_tail_scale[]  (4 elements)
  - tail_shape
CRP_concentration sampler (1)
  - alpha
CRP_cluster_wrapper sampler (10)
  - mean[]  (5 elements)
  - sd[]  (5 elements)
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
thin = 1: alpha, beta_tail_scale, mean, sd, tail_shape, threshold, z
===== Samplers =====
RW sampler (6)
  - threshold
  - beta_tail_scale[]  (4 elements)
  - tail_shape
CRP_concentration sampler (1)
  - alpha
CRP_cluster_wrapper sampler (10)
  - mean[]  (5 elements)
  - sd[]  (5 elements)
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
summary(fit_crp_gpd)

-- Outcome fits --
[control]
MixGPD fit | backend: Chinese Restaurant Process | kernel: Normal Distribution | GPD tail: TRUE
n = 232 | components = 5 | epsilon = 0.025
MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

[treated]
MixGPD fit | backend: Chinese Restaurant Process | kernel: Normal Distribution | GPD tail: TRUE
n = 268 | components = 5 | epsilon = 0.025
MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
params(fit_crp_gpd)
Posterior mean parameters (causal)

[treated]
Posterior mean parameters

$alpha
[1] 0.5376

$w
[1] 0.7923

$mean
[1] 0.4817

$sd
[1] 1.748

$beta_tail_scale
[1]  0.09667 -0.17230  0.12470 -0.11720

$tail_shape
[1] 0.01459

[control]
Posterior mean parameters

$alpha
[1] 0.535

$w
[1] 0.6216 0.3215

$mean
[1] -0.3106  1.5340

$sd
[1] 1.113 1.022

$beta_tail_scale
[1]  0.16220  0.11100 -0.04157 -0.13780

$tail_shape
[1] -0.04116
```

``` r
plot(fit_crp_gpd, family = c("density", "geweke", "caterpillar"))

=== treated ===

=== density ===
```

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-gpd-1.png)

    === geweke ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-gpd-2.png)

    === caterpillar ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-gpd-3.png)

    === control ===

    === density ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-gpd-4.png)

    === geweke ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-gpd-5.png)

    === caterpillar ===

![](v12-causal-same-backend-CRP_files/figure-html/plot-fit-crp-gpd-6.png)

``` r
pred_mean_gpd <- predict(fit_crp_gpd, x = x_eval, type = "mean",
                         interval = "credible", nsim_mean = 100)
plot(pred_mean_gpd)
```

![](v12-causal-same-backend-CRP_files/figure-html/predict-mean-crp-gpd-1.png)![](v12-causal-same-backend-CRP_files/figure-html/predict-mean-crp-gpd-2.png)

``` r
pred_q_gpd <- predict(fit_crp_gpd, x = x_eval, type = "quantile",
                      p = 0.5, interval = "credible")
plot(pred_q_gpd)
```

![](v12-causal-same-backend-CRP_files/figure-html/predict-quantile-crp-gpd-1.png)![](v12-causal-same-backend-CRP_files/figure-html/predict-quantile-crp-gpd-2.png)

``` r
pred_d_gpd <- predict(fit_crp_gpd, x = x_eval, y = y_eval,
                      type = "density", interval = "credible")
plot(pred_d_gpd)
```

![](v12-causal-same-backend-CRP_files/figure-html/predict-density-crp-gpd-1.png)

``` r
pred_p_gpd <- predict(fit_crp_gpd, x = x_eval, y = y_eval,
                      type = "prob", interval = "credible")
plot(pred_p_gpd)
```

![](v12-causal-same-backend-CRP_files/figure-html/predict-prob-crp-gpd-1.png)

``` r
ate_gpd <- ate(fit_crp_gpd, newdata = x_eval,
               interval = "credible", nsim_mean = 100)
plot(ate_gpd)
```

![](v12-causal-same-backend-CRP_files/figure-html/ate-crp-gpd-1.png)![](v12-causal-same-backend-CRP_files/figure-html/ate-crp-gpd-2.png)

``` r
qte_gpd <- qte(fit_crp_gpd, probs = c(0.25, 0.5, 0.75),
               newdata = x_eval, interval = "credible")
plot(qte_gpd)
```

![](v12-causal-same-backend-CRP_files/figure-html/qte-crp-gpd-1.png)![](v12-causal-same-backend-CRP_files/figure-html/qte-crp-gpd-2.png)
