# 19. Causal Inference: Mixed Backends with GPD - Amoroso Kernel

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Causal Inference: Mixed Backends with GPD - Amoroso Kernel

This vignette fits two mixed-backend causal models with GPD tails and an
Amoroso kernel:

- Model A: Treated = SB, Control = CRP
- Model B: Treated = CRP, Control = SB

We shift y to keep outcomes in a stable positive range.

------------------------------------------------------------------------

### Data Setup

``` r

data("causal_alt_real500_p4_k2")
y_raw <- causal_alt_real500_p4_k2$y
T <- causal_alt_real500_p4_k2$T
X <- as.matrix(causal_alt_real500_p4_k2$X)

shift <- abs(min(y_raw)) + 0.1
y <- y_raw + shift

summary_tbl <- tibble(
  statistic = c("N", "Mean", "SD", "Min", "Max"),
  value = c(length(y), mean(y), sd(y), min(y), max(y))
)

summary_tbl %>%
  mutate(value = signif(value, 4)) %>%
  kable(caption = "Shifted Outcome Summary (Amoroso)", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")
```

| statistic | value  |
|:---------:|:------:|
|     N     | 500.00 |
|   Mean    |  8.46  |
|    SD     |  1.76  |
|    Min    |  0.10  |
|    Max    | 13.46  |

Shifted Outcome Summary (Amoroso) {.table .table .table-striped
.table-hover
style="width: auto !important; margin-left: auto; margin-right: auto;"}

``` r

x_eval <- X[1:40, , drop = FALSE]
y_eval <- y[1:40]
u_threshold <- as.numeric(stats::quantile(y, 0.8, names = FALSE))
```

``` r

df_causal <- data.frame(y = y, T = as.factor(T), x1 = X[, 1])

p_scatter <- ggplot(df_causal, aes(x = x1, y = y, color = T)) +
  geom_point(alpha = 0.5) +
  labs(title = "Shifted Outcome vs X1", x = "X1", y = "y (shifted)", color = "Treatment") +
  theme_minimal()

p_scatter
```

![](v19-causal-different-backends-SB_files/figure-html/data-scatter-1.png)

------------------------------------------------------------------------

### Model A: Treated SB, Control CRP (Amoroso + GPD)

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

bundle_sb_crp <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = c("amoroso", "amoroso"),
  backend = c("sb", "crp"),
  PS = "logit",
  GPD = c(TRUE, TRUE),
  components = c(5, 5),
  param_specs = param_specs_gpd,
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 1)
)

bundle_sb_crp
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
       <td style="text-align:center;"> Chinese Restaurant Process </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Kernel </td>
       <td style="text-align:center;"> amoroso </td>
       <td style="text-align:center;"> amoroso </td>
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

summary(bundle_sb_crp)
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
       <td style="text-align:center;"> Chinese Restaurant Process </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Kernel </td>
       <td style="text-align:center;"> amoroso </td>
       <td style="text-align:center;"> amoroso </td>
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

fit_sb_crp <- run_mcmc_causal(bundle_sb_crp)
```

    ===== Monitors =====
    thin = 1: beta
    ===== Samplers =====
    RW sampler (5)
      - beta[]  (5 elements)

    |-------------|-------------|-------------|-------------|
    |-------------------------------------------------------|

    ===== Monitors =====
    thin = 1: alpha, beta_tail_scale, loc, scale, shape1, shape2, tail_shape, threshold, z
    ===== Samplers =====
    RW sampler (6)
      - threshold
      - beta_tail_scale[]  (4 elements)
      - tail_shape
    CRP_concentration sampler (1)
      - alpha
    CRP_cluster_wrapper sampler (20)
      - loc[]  (5 elements)
      - scale[]  (5 elements)
      - shape1[]  (5 elements)
      - shape2[]  (5 elements)
    CRP sampler (1)
      - z[1:232] 

      [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.

    ===== Monitors =====
    thin = 1: alpha, beta_loc, beta_ps_loc, beta_ps_scale, beta_scale, beta_tail_scale, shape1, shape2, tail_shape, threshold, w, z
    ===== Samplers =====
    RW sampler (71)
      - alpha
      - shape1[]  (5 elements)
      - shape2[]  (5 elements)
      - beta_loc[]  (20 elements)
      - beta_ps_loc[]  (5 elements)
      - beta_scale[]  (20 elements)
      - beta_ps_scale[]  (5 elements)
      - threshold
      - beta_tail_scale[]  (4 elements)
      - tail_shape
      - v[]  (4 elements)
    categorical sampler (268)
      - z[]  (268 elements)

``` r

summary(fit_sb_crp)
```

    -- PS fit --
    DPmixGPD PS fit
    model: logit 

    -- Outcome fits --
    [control]
    MixGPD fit | backend: Chinese Restaurant Process | kernel: Amoroso Distribution | GPD tail: TRUE
    n = 232 | components = 5 | epsilon = 0.025
    MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
    Fit
    Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

    [treated]
    MixGPD fit | backend: Stick-Breaking Process | kernel: Amoroso Distribution | GPD tail: TRUE
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
    [1] "0.53"

    $w
    [1] "0.995"

    $beta_loc
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
       <td style="text-align:center;"> 0.061 </td>
       <td style="text-align:center;"> -1.243 </td>
       <td style="text-align:center;"> -0.182 </td>
       <td style="text-align:center;"> 0.235 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> 0.214 </td>
       <td style="text-align:center;"> 0.472 </td>
       <td style="text-align:center;"> -1.306 </td>
       <td style="text-align:center;"> 0.836 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> 0.147 </td>
       <td style="text-align:center;"> -0.25 </td>
       <td style="text-align:center;"> 0.124 </td>
       <td style="text-align:center;"> 0.051 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> -0.094 </td>
       <td style="text-align:center;"> -0.085 </td>
       <td style="text-align:center;"> -0.02 </td>
       <td style="text-align:center;"> 0.097 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> 0.172 </td>
       <td style="text-align:center;"> 0.075 </td>
       <td style="text-align:center;"> 0.151 </td>
       <td style="text-align:center;"> 0.158 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_loc
    [1] "0.559"  "0.455"  "-0.229" "0.269"  "-0.031"

    $beta_scale
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
       <td style="text-align:center;"> -0.24 </td>
       <td style="text-align:center;"> -0.012 </td>
       <td style="text-align:center;"> 0.136 </td>
       <td style="text-align:center;"> -0.169 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> -0.488 </td>
       <td style="text-align:center;"> 0.837 </td>
       <td style="text-align:center;"> 0.28 </td>
       <td style="text-align:center;"> -0.097 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> -0.198 </td>
       <td style="text-align:center;"> -0.037 </td>
       <td style="text-align:center;"> -0.151 </td>
       <td style="text-align:center;"> -0.098 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> 0.221 </td>
       <td style="text-align:center;"> -0.02 </td>
       <td style="text-align:center;"> 0.056 </td>
       <td style="text-align:center;"> -0.004 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> -0.128 </td>
       <td style="text-align:center;"> 0.01 </td>
       <td style="text-align:center;"> -0.026 </td>
       <td style="text-align:center;"> -0.282 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_scale
    [1] "0.492"  "-0.675" "-0.42"  "0.133"  "0.057" 

    $shape1
    [1] "12.379"

    $shape2
    [1] "1.169"

    $beta_tail_scale
    [1] "0.096"  "-0.163" "0.083"  "0.089" 

    $tail_shape
    [1] "0.027"

    [control]
    Posterior mean parameters

    $alpha
    [1] "0.213"

    $w
    [1] "0.958"

    $loc
    [1] "4.355"

    $scale
    [1] "1.714"

    $shape1
    [1] "3.467"

    $shape2
    [1] "1.418"

    $beta_tail_scale
    [1] "0.282"  "0.147"  "0.01"   "-0.217"

    $tail_shape
    [1] "0.026"

``` r

if (interactive()) plot(fit_sb_crp, family = c("traceplot", "autocorrelation", "running"))
```

``` r

pred_mean_sb_crp <- predict(fit_sb_crp, x = x_eval, type = "mean",
                            interval = "credible", nsim_mean = 100)
if (interactive()) plot(pred_mean_sb_crp)
```

``` r

pred_q_sb_crp <- predict(fit_sb_crp, x = x_eval, type = "quantile",
                         p = 0.5, interval = "credible")
if (interactive()) plot(pred_q_sb_crp)
```

``` r

pred_d_sb_crp <- predict(fit_sb_crp, x = x_eval, y = y_eval,
                         type = "density", interval = "credible")
if (interactive()) plot(pred_d_sb_crp)
```

``` r

pred_surv_sb_crp <- predict(fit_sb_crp, x = x_eval, y = y_eval,
                            type = "survival", interval = "credible")
if (interactive()) plot(pred_surv_sb_crp)
```

``` r

ate_sb_crp <- ate(fit_sb_crp, newdata = x_eval,
                  interval = "credible", nsim_mean = 100)
if (interactive()) plot(ate_sb_crp)
```

``` r

qte_sb_crp <- qte(fit_sb_crp, probs = c(0.25, 0.5, 0.75),
                  newdata = x_eval, interval = "credible")
if (interactive()) plot(qte_sb_crp)
```

------------------------------------------------------------------------

### Model B: Treated CRP, Control SB (Amoroso + GPD)

``` r

bundle_crp_sb <- build_causal_bundle(
  y = y,
  T = T,
  X = X,
  kernel = c("amoroso", "amoroso"),
  backend = c("crp", "sb"),
  PS = "logit",
  GPD = c(TRUE, TRUE),
  components = c(5, 5),
  param_specs = param_specs_gpd,
  mcmc_outcome = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2),
  mcmc_ps = list(niter = 2500, nburnin = 500, nchains = 1, thin = 1, seed = 2)
)

bundle_crp_sb
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
       <td style="text-align:center;"> Chinese Restaurant Process </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Kernel </td>
       <td style="text-align:center;"> amoroso </td>
       <td style="text-align:center;"> amoroso </td>
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

summary(bundle_crp_sb)
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
       <td style="text-align:center;"> Chinese Restaurant Process </td>
       <td style="text-align:center;"> Stick-Breaking Process </td>
      </tr>
      <tr>
       <td style="text-align:center;"> Kernel </td>
       <td style="text-align:center;"> amoroso </td>
       <td style="text-align:center;"> amoroso </td>
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

fit_crp_sb <- run_mcmc_causal(bundle_crp_sb)
```

    ===== Monitors =====
    thin = 1: beta
    ===== Samplers =====
    RW sampler (5)
      - beta[]  (5 elements)

    |-------------|-------------|-------------|-------------|
    |-------------------------------------------------------|

    ===== Monitors =====
    thin = 1: alpha, beta_loc, beta_ps_loc, beta_ps_scale, beta_scale, beta_tail_scale, shape1, shape2, tail_shape, threshold, w, z
    ===== Samplers =====
    RW sampler (71)
      - alpha
      - shape1[]  (5 elements)
      - shape2[]  (5 elements)
      - beta_loc[]  (20 elements)
      - beta_ps_loc[]  (5 elements)
      - beta_scale[]  (20 elements)
      - beta_ps_scale[]  (5 elements)
      - threshold
      - beta_tail_scale[]  (4 elements)
      - tail_shape
      - v[]  (4 elements)
    categorical sampler (232)
      - z[]  (232 elements)

    ===== Monitors =====
    thin = 1: alpha, beta_tail_scale, loc, scale, shape1, shape2, tail_shape, threshold, z
    ===== Samplers =====
    RW sampler (6)
      - threshold
      - beta_tail_scale[]  (4 elements)
      - tail_shape
    CRP_concentration sampler (1)
      - alpha
    CRP_cluster_wrapper sampler (20)
      - loc[]  (5 elements)
      - scale[]  (5 elements)
      - shape1[]  (5 elements)
      - shape2[]  (5 elements)
    CRP sampler (1)
      - z[1:268] 

      [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.

``` r

summary(fit_crp_sb)
```

    -- PS fit --
    DPmixGPD PS fit
    model: logit 

    -- Outcome fits --
    [control]
    MixGPD fit | backend: Stick-Breaking Process | kernel: Amoroso Distribution | GPD tail: TRUE
    n = 232 | components = 5 | epsilon = 0.025
    MCMC: niter=2500, nburnin=500, thin=1, nchains=1 
    Fit
    Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

    [treated]
    MixGPD fit | backend: Chinese Restaurant Process | kernel: Amoroso Distribution | GPD tail: TRUE
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
    [1] "0.691"

    $w
    [1] "0.576" "0.281"

    $loc
    [1] "5.372" "5.853"

    $scale
    [1] "1.399" "2.547"

    $shape1
    [1] "3.334" "2.585"

    $shape2
    [1] "1.432" "3.089"

    $beta_tail_scale
    [1] "0.068"  "-0.026" "0.101"  "-0.118"

    $tail_shape
    [1] "-0.07"

    [control]
    Posterior mean parameters

    $alpha
    [1] "0.523"

    $w
    [1] "0.807"

    $beta_loc
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
       <td style="text-align:center;"> 0.051 </td>
       <td style="text-align:center;"> 0.009 </td>
       <td style="text-align:center;"> -0.04 </td>
       <td style="text-align:center;"> 0.335 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> -0.168 </td>
       <td style="text-align:center;"> 0.107 </td>
       <td style="text-align:center;"> 0.507 </td>
       <td style="text-align:center;"> 0.044 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> -0.363 </td>
       <td style="text-align:center;"> -0.699 </td>
       <td style="text-align:center;"> -0.424 </td>
       <td style="text-align:center;"> 1.868 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> -0.126 </td>
       <td style="text-align:center;"> -0.165 </td>
       <td style="text-align:center;"> 0.151 </td>
       <td style="text-align:center;"> 0.038 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> 0.042 </td>
       <td style="text-align:center;"> -0.097 </td>
       <td style="text-align:center;"> -0.001 </td>
       <td style="text-align:center;"> 0.249 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_loc
    [1] "-0.149" "-0.208" "0.266"  "-0.126" "-0.033"

    $beta_scale
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
       <td style="text-align:center;"> -0.293 </td>
       <td style="text-align:center;"> -0.109 </td>
       <td style="text-align:center;"> 0.197 </td>
       <td style="text-align:center;"> 0.709 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> -0.307 </td>
       <td style="text-align:center;"> -0.183 </td>
       <td style="text-align:center;"> -0.237 </td>
       <td style="text-align:center;"> 0.322 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> -0.848 </td>
       <td style="text-align:center;"> -0.58 </td>
       <td style="text-align:center;"> 0.413 </td>
       <td style="text-align:center;"> -0.741 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> -0.096 </td>
       <td style="text-align:center;"> -0.104 </td>
       <td style="text-align:center;"> -0.213 </td>
       <td style="text-align:center;"> 0.152 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> -0.035 </td>
       <td style="text-align:center;"> -0.016 </td>
       <td style="text-align:center;"> -0.094 </td>
       <td style="text-align:center;"> 0.038 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_scale
    [1] "0.153"  "-0.09"  "1.929"  "-0.018" "-0.111"

    $shape1
    [1] "8.717"

    $shape2
    [1] "0.699"

    $beta_tail_scale
    [1] "-0.093" "0.01"   "-0.06"  "1.074" 

    $tail_shape
    [1] "-0.001"

``` r

if (interactive()) plot(fit_crp_sb, family = c("density", "geweke", "caterpillar"))
```

``` r

pred_mean_crp_sb <- predict(fit_crp_sb, x = x_eval, type = "mean",
                            interval = "credible", nsim_mean = 100)
if (interactive()) plot(pred_mean_crp_sb)
```

``` r

pred_q_crp_sb <- predict(fit_crp_sb, x = x_eval, type = "quantile",
                         p = 0.5, interval = "credible")
if (interactive()) plot(pred_q_crp_sb)
```

``` r

pred_d_crp_sb <- predict(fit_crp_sb, x = x_eval, y = y_eval,
                         type = "density", interval = "credible")
if (interactive()) plot(pred_d_crp_sb)
```

``` r

pred_surv_crp_sb <- predict(fit_crp_sb, x = x_eval, y = y_eval,
                            type = "survival", interval = "credible")
if (interactive()) plot(pred_surv_crp_sb)
```

``` r

ate_crp_sb <- ate(fit_crp_sb, newdata = x_eval,
                  interval = "credible", nsim_mean = 100)
if (interactive()) plot(ate_crp_sb)
```

``` r

qte_crp_sb <- qte(fit_crp_sb, probs = c(0.25, 0.5, 0.75),
                  newdata = x_eval, interval = "credible")
if (interactive()) plot(qte_crp_sb)
```
