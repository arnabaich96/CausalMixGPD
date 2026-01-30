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

| statistic |  value  |
|:---------:|:-------:|
|     N     | 500.000 |
|   Mean    |  0.274  |
|    SD     |  1.764  |
|    Min    | -8.089  |
|    Max    |  5.275  |

Outcome Summary (Cauchy) {.table .table .table-striped .table-hover
style="width: auto !important; margin-left: auto; margin-right: auto;"}

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
       <td style="text-align:center;"> cauchy </td>
       <td style="text-align:center;"> cauchy </td>
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
       <td style="text-align:center;"> cauchy </td>
       <td style="text-align:center;"> cauchy </td>
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
    thin = 1: alpha, location, scale, z
    ===== Samplers =====
    CRP_concentration sampler (1)
      - alpha
    CRP_cluster_wrapper sampler (10)
      - location[]  (5 elements)
      - scale[]  (5 elements)
    CRP sampler (1)
      - z[1:232] 

      [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.

    ===== Monitors =====
    thin = 1: alpha, beta_location, beta_ps_location, scale, w, z
    ===== Samplers =====
    RW sampler (35)
      - alpha
      - scale[]  (5 elements)
      - beta_location[]  (20 elements)
      - beta_ps_location[]  (5 elements)
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
    [1] "1.89"

    $w
    [1] "0.37"  "0.249" "0.192"

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
       <td style="text-align:center;"> 0.134 </td>
       <td style="text-align:center;"> -0.822 </td>
       <td style="text-align:center;"> 0.699 </td>
       <td style="text-align:center;"> 0.717 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> -0.512 </td>
       <td style="text-align:center;"> 0.661 </td>
       <td style="text-align:center;"> 0.912 </td>
       <td style="text-align:center;"> 1.235 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> -0.811 </td>
       <td style="text-align:center;"> 0.572 </td>
       <td style="text-align:center;"> 0.085 </td>
       <td style="text-align:center;"> -0.783 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> 0.404 </td>
       <td style="text-align:center;"> -1.122 </td>
       <td style="text-align:center;"> 0.07 </td>
       <td style="text-align:center;"> 0.48 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> -0.026 </td>
       <td style="text-align:center;"> -0.705 </td>
       <td style="text-align:center;"> -0.908 </td>
       <td style="text-align:center;"> 0.336 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_location
    [1] "0.524"  "0.467"  "0.856"  "0.422"  "-0.441"

    $scale
    [1] "0.653" "0.565" "0.53" 

    [control]
    Posterior mean parameters

    $alpha
    [1] "0.632"

    $w
    [1] "0.417" "0.302" "0.219"

    $location
    [1] "-0.046" "0.662"  "-0.17" 

    $scale
    [1] "0.524" "0.523" "0.418"

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
       <td style="text-align:center;"> cauchy </td>
       <td style="text-align:center;"> cauchy </td>
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
       <td style="text-align:center;"> cauchy </td>
       <td style="text-align:center;"> cauchy </td>
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
    thin = 1: alpha, beta_location, beta_ps_location, scale, w, z
    ===== Samplers =====
    RW sampler (35)
      - alpha
      - scale[]  (5 elements)
      - beta_location[]  (20 elements)
      - beta_ps_location[]  (5 elements)
      - v[]  (4 elements)
    categorical sampler (232)
      - z[]  (232 elements)

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

      [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.

``` r

summary(fit_crp_sb)
```

    -- PS fit --
    DPmixGPD PS fit
    model: logit 

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
    [1] "0.462"

    $w
    [1] "0.526" "0.436"

    $location
    [1] "0.674"  "-0.155"

    $scale
    [1] "0.627" "0.522"

    [control]
    Posterior mean parameters

    $alpha
    [1] "1.163"

    $w
    [1] "0.441" "0.311" "0.209"

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
       <td style="text-align:center;"> 0.745 </td>
       <td style="text-align:center;"> 0.301 </td>
       <td style="text-align:center;"> -0.86 </td>
       <td style="text-align:center;"> -0.321 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp2 </td>
       <td style="text-align:center;"> 1.087 </td>
       <td style="text-align:center;"> 0.918 </td>
       <td style="text-align:center;"> -0.52 </td>
       <td style="text-align:center;"> -0.032 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp3 </td>
       <td style="text-align:center;"> 0.085 </td>
       <td style="text-align:center;"> 0.023 </td>
       <td style="text-align:center;"> -0.235 </td>
       <td style="text-align:center;"> 2.471 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp4 </td>
       <td style="text-align:center;"> 0.104 </td>
       <td style="text-align:center;"> 0.364 </td>
       <td style="text-align:center;"> 0.011 </td>
       <td style="text-align:center;"> 0.179 </td>
      </tr>
      <tr>
       <td style="text-align:left;"> comp5 </td>
       <td style="text-align:center;"> -0.729 </td>
       <td style="text-align:center;"> 0.61 </td>
       <td style="text-align:center;"> -0.206 </td>
       <td style="text-align:center;"> 0.869 </td>
      </tr>
    </tbody>
    </table>
    $beta_ps_location
    [1] "-1.636" "-2.537" "-0.81"  "0.196"  "-0.122"

    $scale
    [1] "0.685" "0.612" "0.495"

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
