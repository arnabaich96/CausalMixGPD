# 17. Causal Inference: Same Backend (SB) - Laplace Kernel

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

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

| statistic |  value  |
|:---------:|:-------:|
|     N     | 500.000 |
|   Mean    |  0.274  |
|    SD     |  1.764  |
|    Min    | -8.089  |
|    Max    |  5.275  |

Causal Dataset Summary {.table .table .table-striped .table-hover
style="width: auto !important; margin-left: auto; margin-right: auto;"}

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

![](v17-causal-same-backend-SB_files/figure-html/data-scatter-1.png)

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
  components = 5,
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1)
)

bundle_sb_bulk
```

    DPmixGPD causal bundle
    PS model: Bayesian logit (T | X) 
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:center;"> Field </th>
       <th style="text-align:center;"> Treated </th>
       <th style="text-align:center;"> Control </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:center;"> Backend </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Kernel </td>
       <td style="text-align:center;"> laplace </td>
       <td style="text-align:center;"> laplace </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Components </td>
       <td style="text-align:center;"> 5 </td>
       <td style="text-align:center;"> 5 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> GPD tail </td>
       <td style="text-align:center;"> FALSE </td>
       <td style="text-align:center;"> FALSE </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Epsilon </td>
       <td style="text-align:center;"> 0.025 </td>
       <td style="text-align:center;"> 0.025 </td>
      </tr>
    </tbody>
    </table>
    Outcome PS included: TRUE 
    n (control) = 232 | n (treated) = 268 

``` r

summary(bundle_sb_bulk)
```

    DPmixGPD causal bundle summary
    DPmixGPD causal bundle
    PS model: Bayesian logit (T | X) 
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:center;"> Field </th>
       <th style="text-align:center;"> Treated </th>
       <th style="text-align:center;"> Control </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:center;"> Backend </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Kernel </td>
       <td style="text-align:center;"> laplace </td>
       <td style="text-align:center;"> laplace </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Components </td>
       <td style="text-align:center;"> 5 </td>
       <td style="text-align:center;"> 5 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> GPD tail </td>
       <td style="text-align:center;"> FALSE </td>
       <td style="text-align:center;"> FALSE </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Epsilon </td>
       <td style="text-align:center;"> 0.025 </td>
       <td style="text-align:center;"> 0.025 </td>
      </tr>
    </tbody>
    </table>
    Outcome PS included: TRUE 
    n (control) = 232 | n (treated) = 268 

``` r

fit_sb_bulk <- run_mcmc_causal(bundle_sb_bulk)
```

    ===== Monitors =====
    thin = 1: beta
    ===== Samplers =====
    RW sampler (5)
      - beta[]  (5 elements)

    |-------------|-------------|-------------|-------------|
    |-------------------------------------------------------|

    ===== Monitors =====
    thin = 1: alpha, beta_location, beta_ps_location, scale, w, z
    ===== Samplers =====
    RW sampler (30)
      - alpha
      - beta_location[]  (20 elements)
      - beta_ps_location[]  (5 elements)
      - v[]  (4 elements)
    conjugate sampler (5)
      - scale[]  (5 elements)
    categorical sampler (232)
      - z[]  (232 elements)

    ===== Monitors =====
    thin = 1: alpha, beta_location, beta_ps_location, scale, w, z
    ===== Samplers =====
    RW sampler (30)
      - alpha
      - beta_location[]  (20 elements)
      - beta_ps_location[]  (5 elements)
      - v[]  (4 elements)
    conjugate sampler (5)
      - scale[]  (5 elements)
    categorical sampler (268)
      - z[]  (268 elements)

``` r

summary(fit_sb_bulk)
```

    -- PS fit --
    DPmixGPD PS fit
    model: logit 

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

``` r

params(fit_sb_bulk)
```

    Posterior mean parameters (causal)

    [treated]
    Posterior mean parameters

    $alpha
    [1] "2.302"

    $w
    [1] "0.425" "0.277" "0.167"

    $beta_location
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:left;">   </th>
       <th style="text-align:center;"> x1 </th>
       <th style="text-align:center;"> x2 </th>
       <th style="text-align:center;"> x3 </th>
       <th style="text-align:center;"> x4 </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:left;"> comp1 </td>
       <td style="text-align:center;"> 0.784 </td>
       <td style="text-align:center;"> -0.463 </td>
       <td style="text-align:center;"> -0.367 </td>
       <td style="text-align:center;"> 0.423 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> -0.095 </td>
       <td style="text-align:center;"> -0.912 </td>
       <td style="text-align:center;"> -0.267 </td>
       <td style="text-align:center;"> 0.413 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> 0.123 </td>
       <td style="text-align:center;"> 0.957 </td>
       <td style="text-align:center;"> 0.151 </td>
       <td style="text-align:center;"> 0.093 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> 0.259 </td>
       <td style="text-align:center;"> -0.217 </td>
       <td style="text-align:center;"> 0.189 </td>
       <td style="text-align:center;"> -0.185 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> 0.196 </td>
       <td style="text-align:center;"> -0.008 </td>
       <td style="text-align:center;"> 0.386 </td>
       <td style="text-align:center;"> 0.824 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_location
    [1] "-0.909" "0.361"  "-0.23"  "0.111"  "0.01"  

    $scale
    [1] "0.863" "1.057" "1.493"

    [control]
    Posterior mean parameters

    $alpha
    [1] "0.957"

    $w
    [1] "0.505" "0.334"

    $beta_location
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:left;">   </th>
       <th style="text-align:center;"> x1 </th>
       <th style="text-align:center;"> x2 </th>
       <th style="text-align:center;"> x3 </th>
       <th style="text-align:center;"> x4 </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:left;"> comp1 </td>
       <td style="text-align:center;"> -0.196 </td>
       <td style="text-align:center;"> -0.038 </td>
       <td style="text-align:center;"> -0.248 </td>
       <td style="text-align:center;"> 1.827 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> 0.472 </td>
       <td style="text-align:center;"> 0.183 </td>
       <td style="text-align:center;"> -0.426 </td>
       <td style="text-align:center;"> -0.217 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> 0.221 </td>
       <td style="text-align:center;"> 0.405 </td>
       <td style="text-align:center;"> -0.276 </td>
       <td style="text-align:center;"> 0.527 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> 0.14 </td>
       <td style="text-align:center;"> -0.051 </td>
       <td style="text-align:center;"> 0.024 </td>
       <td style="text-align:center;"> 0.538 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> -0.551 </td>
       <td style="text-align:center;"> 0.197 </td>
       <td style="text-align:center;"> -0.166 </td>
       <td style="text-align:center;"> -0.241 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_location
    [1] "-0.21"  "-1.001" "-0.617" "0.064"  "0.25"  

    $scale
    [1] "0.977" "1.164"

``` r

if (interactive()) plot(fit_sb_bulk, family = c("traceplot", "autocorrelation", "running"))
```

``` r

pred_mean_bulk <- predict(fit_sb_bulk, x = x_eval, type = "mean",
                          interval = "credible", nsim_mean = 100)
if (interactive()) plot(pred_mean_bulk)
```

``` r

pred_q_bulk <- predict(fit_sb_bulk, x = x_eval, type = "quantile",
                       p = 0.5, interval = "credible")
if (interactive()) plot(pred_q_bulk)
```

``` r

pred_d_bulk <- predict(fit_sb_bulk, x = x_eval, y = y_eval,
                       type = "density", interval = "credible")
if (interactive()) plot(pred_d_bulk)
```

``` r

pred_surv_bulk <- predict(fit_sb_bulk, x = x_eval, y = y_eval,
                          type = "survival", interval = "credible")
if (interactive()) plot(pred_surv_bulk)
```

``` r

ate_bulk <- ate(fit_sb_bulk, newdata = x_eval,
                interval = "credible", nsim_mean = 100)
if (interactive()) plot(ate_bulk)
```

``` r

qte_bulk <- qte(fit_sb_bulk, probs = c(0.25, 0.5, 0.75),
                newdata = x_eval, interval = "credible")
if (interactive()) plot(qte_bulk)
```

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
  components = 5,
  param_specs = param_specs_gpd,
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2)
)

bundle_sb_gpd
```

    DPmixGPD causal bundle
    PS model: Bayesian logit (T | X) 
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:center;"> Field </th>
       <th style="text-align:center;"> Treated </th>
       <th style="text-align:center;"> Control </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:center;"> Backend </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Kernel </td>
       <td style="text-align:center;"> laplace </td>
       <td style="text-align:center;"> laplace </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Components </td>
       <td style="text-align:center;"> 5 </td>
       <td style="text-align:center;"> 5 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> GPD tail </td>
       <td style="text-align:center;"> TRUE </td>
       <td style="text-align:center;"> TRUE </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Epsilon </td>
       <td style="text-align:center;"> 0.025 </td>
       <td style="text-align:center;"> 0.025 </td>
      </tr>
    </tbody>
    </table>
    Outcome PS included: TRUE 
    n (control) = 232 | n (treated) = 268 

``` r

summary(bundle_sb_gpd)
```

    DPmixGPD causal bundle summary
    DPmixGPD causal bundle
    PS model: Bayesian logit (T | X) 
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:center;"> Field </th>
       <th style="text-align:center;"> Treated </th>
       <th style="text-align:center;"> Control </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:center;"> Backend </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Kernel </td>
       <td style="text-align:center;"> laplace </td>
       <td style="text-align:center;"> laplace </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Components </td>
       <td style="text-align:center;"> 5 </td>
       <td style="text-align:center;"> 5 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> GPD tail </td>
       <td style="text-align:center;"> TRUE </td>
       <td style="text-align:center;"> TRUE </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Epsilon </td>
       <td style="text-align:center;"> 0.025 </td>
       <td style="text-align:center;"> 0.025 </td>
      </tr>
    </tbody>
    </table>
    Outcome PS included: TRUE 
    n (control) = 232 | n (treated) = 268 

``` r

fit_sb_gpd <- run_mcmc_causal(bundle_sb_gpd)
```

    ===== Monitors =====
    thin = 1: beta
    ===== Samplers =====
    RW sampler (5)
      - beta[]  (5 elements)

    |-------------|-------------|-------------|-------------|
    |-------------------------------------------------------|

    ===== Monitors =====
    thin = 1: alpha, beta_location, beta_ps_location, beta_tail_scale, scale, tail_shape, threshold, w, z
    ===== Samplers =====
    RW sampler (41)
      - alpha
      - scale[]  (5 elements)
      - beta_location[]  (20 elements)
      - beta_ps_location[]  (5 elements)
      - threshold
      - beta_tail_scale[]  (4 elements)
      - tail_shape
      - v[]  (4 elements)
    categorical sampler (232)
      - z[]  (232 elements)

    ===== Monitors =====
    thin = 1: alpha, beta_location, beta_ps_location, beta_tail_scale, scale, tail_shape, threshold, w, z
    ===== Samplers =====
    RW sampler (41)
      - alpha
      - scale[]  (5 elements)
      - beta_location[]  (20 elements)
      - beta_ps_location[]  (5 elements)
      - threshold
      - beta_tail_scale[]  (4 elements)
      - tail_shape
      - v[]  (4 elements)
    categorical sampler (268)
      - z[]  (268 elements)

``` r

summary(fit_sb_gpd)
```

    -- PS fit --
    DPmixGPD PS fit
    model: logit 

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

``` r

params(fit_sb_gpd)
```

    Posterior mean parameters (causal)

    [treated]
    Posterior mean parameters

    $alpha
    [1] "0.883"

    $w
    [1] "0.47"  "0.315" "0.168"

    $beta_location
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:left;">   </th>
       <th style="text-align:center;"> x1 </th>
       <th style="text-align:center;"> x2 </th>
       <th style="text-align:center;"> x3 </th>
       <th style="text-align:center;"> x4 </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:left;"> comp1 </td>
       <td style="text-align:center;"> 0.116 </td>
       <td style="text-align:center;"> -0.609 </td>
       <td style="text-align:center;"> -0.097 </td>
       <td style="text-align:center;"> 0.771 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> -0.554 </td>
       <td style="text-align:center;"> 0.415 </td>
       <td style="text-align:center;"> 0.039 </td>
       <td style="text-align:center;"> 0.339 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> -0.014 </td>
       <td style="text-align:center;"> 0.097 </td>
       <td style="text-align:center;"> 0.557 </td>
       <td style="text-align:center;"> 1.684 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> -0.143 </td>
       <td style="text-align:center;"> 0.175 </td>
       <td style="text-align:center;"> 0.016 </td>
       <td style="text-align:center;"> 0.438 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> 0.386 </td>
       <td style="text-align:center;"> -0.152 </td>
       <td style="text-align:center;"> -0.159 </td>
       <td style="text-align:center;"> 0.524 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_location
    [1] "0.428"  "0.583"  "1.023"  "0.13"   "-0.305"

    $scale
    [1] "1.325" "1.338" "1.493"

    $beta_tail_scale
    [1] "0.033"  "-0.141" "0.161"  "-0.048"

    $tail_shape
    [1] "0.026"

    [control]
    Posterior mean parameters

    $alpha
    [1] "1.12"

    $w
    [1] "0.56"  "0.308"

    $beta_location
    <table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
     <thead>
      <tr>
       <th style="text-align:left;">   </th>
       <th style="text-align:center;"> x1 </th>
       <th style="text-align:center;"> x2 </th>
       <th style="text-align:center;"> x3 </th>
       <th style="text-align:center;"> x4 </th>
      </tr>
     </thead>
    <tbody>
      <tr>
       <td style="text-align:left;"> comp1 </td>
       <td style="text-align:center;"> 1.182 </td>
       <td style="text-align:center;"> 0.759 </td>
       <td style="text-align:center;"> -0.647 </td>
       <td style="text-align:center;"> 0.202 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> -0.824 </td>
       <td style="text-align:center;"> 0.043 </td>
       <td style="text-align:center;"> -0.127 </td>
       <td style="text-align:center;"> 1.994 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> -0.045 </td>
       <td style="text-align:center;"> -0.438 </td>
       <td style="text-align:center;"> -0.099 </td>
       <td style="text-align:center;"> 0.643 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> -0.137 </td>
       <td style="text-align:center;"> -0.145 </td>
       <td style="text-align:center;"> 0.413 </td>
       <td style="text-align:center;"> -0.102 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> -0.296 </td>
       <td style="text-align:center;"> -0.17 </td>
       <td style="text-align:center;"> -0.588 </td>
       <td style="text-align:center;"> 0.299 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_location
    [1] "-2.575" "0.943"  "-0.036" "0.15"   "-0.275"

    $scale
    [1] "1.052" "1.167"

    $beta_tail_scale
    [1] "0.306"  "0.202"  "0.098"  "-0.083"

    $tail_shape
    [1] "-0.037"

``` r

if (interactive()) plot(fit_sb_gpd, family = c("density", "geweke", "caterpillar"))
```

``` r

pred_mean_gpd <- predict(fit_sb_gpd, x = x_eval, type = "mean",
                         interval = "credible", nsim_mean = 100)
if (interactive()) plot(pred_mean_gpd)
```

``` r

pred_q_gpd <- predict(fit_sb_gpd, x = x_eval, type = "quantile",
                      p = 0.5, interval = "credible")
if (interactive()) plot(pred_q_gpd)
```

``` r

pred_d_gpd <- predict(fit_sb_gpd, x = x_eval, y = y_eval,
                      type = "density", interval = "credible")
if (interactive()) plot(pred_d_gpd)
```

``` r

pred_surv_gpd <- predict(fit_sb_gpd, x = x_eval, y = y_eval,
                         type = "survival", interval = "credible")
if (interactive()) plot(pred_surv_gpd)
```

``` r

ate_gpd <- ate(fit_sb_gpd, newdata = x_eval,
               interval = "credible", nsim_mean = 100)
if (interactive()) plot(ate_gpd)
```

``` r

qte_gpd <- qte(fit_sb_gpd, probs = c(0.25, 0.5, 0.75),
               newdata = x_eval, interval = "credible")
if (interactive()) plot(qte_gpd)
```
