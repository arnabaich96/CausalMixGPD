# 7. Unconditional DPmixGPD with Stick-Breaking Backend

## Unconditional DPmixGPD: Stick-Breaking (SB) Backend with Tail Augmentation

**Purpose**: Demonstrate the stick-breaking backend (`components = J`)
while augmenting the extreme tail with a GPD. This mirrors the CRP+GPD
pipeline (v06) but highlights the fixed-truncation behavior for bulk
components.

------------------------------------------------------------------------

### Data Setup

``` r
# Tail-heavy data
data("nc_pos_tail200_k4")
y_tail <- nc_pos_tail200_k4$y

summary_tbl <- tibble(
  statistic = c("N", "Mean", "SD", "Min", "Max"),
  value = c(length(y_tail), mean(y_tail), sd(y_tail), min(y_tail), max(y_tail))
)

df_data <- data.frame(y = y_tail)

p_raw <- ggplot(df_data, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "darkorange", alpha = 0.6, color = "black") +
  geom_density(color = "darkred", linewidth = 1) +
  labs(title = "Tail-Designed Data", x = "y", y = "Density") +
  theme_minimal()

grid.arrange(p_raw, ncol = 1)
```

![](v07-unconditional-DPmixGPD-SB_files/figure-html/data-setup-1.png)

| statistic |  value   |
|:---------:|:--------:|
|     N     | 200.0000 |
|   Mean    |  2.3340  |
|    SD     |  2.3000  |
|    Min    |  0.3283  |
|    Max    | 19.8700  |

Tail Dataset Summaries

------------------------------------------------------------------------

### Threshold Selection

``` r
thresholds <- quantile(y_tail, c(0.70, 0.75, 0.80, 0.85))
u_threshold <- thresholds["80%"]

ggplot(df_data, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "skyblue", alpha = 0.6, color = "black") +
  geom_vline(xintercept = u_threshold, linetype = "dashed", color = "black") +
  labs(title = paste("Threshold at", round(u_threshold, 2)), x = "y", y = "Density") +
  theme_minimal()
```

![](v07-unconditional-DPmixGPD-SB_files/figure-html/threshold-1.png)

------------------------------------------------------------------------

### Model Specification & Bundle

This follows the same structure as the SB bulk-only vignette (`v05`):
build a bundle with
[`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md),
run MCMC, then use the S3
[`predict()`](https://rdrr.io/r/stats/predict.html) +
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) helpers.
Compared with `v06` (CRP+GPD), here we keep the **stick-breaking**
backend and use a **Gamma** bulk kernel with a lognormal threshold
prior, then contrast it with a bulk-only **Laplace** fit.

``` r
bundle_sb_gpd <- build_nimble_bundle(
  y = y_tail,
  kernel = "gamma",
  backend = "sb",
  GPD = TRUE,
  components = 5,
  param_specs = list(
    gpd = list(
      threshold = list(
        mode = "dist",
        dist = "lognormal",
        args = list(meanlog = log(max(u_threshold, .Machine$double.eps)), sdlog = 0.25)
      )
    )
  ),
  mcmc = list(
    niter = 60,
    nburnin = 10,
    nchains = 2,
    thin = 1
  )
)
```

------------------------------------------------------------------------

### Running MCMC

``` r
fit_sb_gpd <- run_mcmc_bundle_manual(bundle_sb_gpd)
[MCMC] Creating NIMBLE model...
[MCMC] NIMBLE model created successfully.
[MCMC] Configuring MCMC...
===== Monitors =====
thin = 1: alpha, scale, shape, tail_scale, tail_shape, threshold, w, z
===== Samplers =====
RW sampler (18)
  - alpha
  - shape[]  (5 elements)
  - scale[]  (5 elements)
  - threshold
  - tail_scale
  - tail_shape
  - v[]  (4 elements)
categorical sampler (200)
  - z[]  (200 elements)
[MCMC] MCMC configured.
[MCMC] Building MCMC object...
[MCMC] MCMC object built.
[MCMC] Attempting NIMBLE compilation (this may take a minute)...
[MCMC] Compiling model...
[MCMC] Compiling MCMC sampler...
[MCMC] Compilation successful.
|-------------|-------------|-------------|-------------|
|-------------------------------------------------------|
|-------------|-------------|-------------|-------------|
|-------------------------------------------------------|
[MCMC] MCMC execution complete. Processing results...
summary(fit_sb_gpd)
MixGPD summary | backend: Stick-Breaking Process | kernel: Gamma Distribution | GPD tail: TRUE | epsilon: 0.025
n = 200 | components = 5
Summary
Initial components: 5 | Components after truncation: 3

WAIC: 689.787
lppd: -309.797 | pWAIC: 35.097

Summary table
  parameter  mean    sd q0.025 q0.500 q0.975    ess
 weights[1] 0.434 0.046  0.345  0.440  0.508  3.460
 weights[2] 0.360 0.039  0.282  0.360  0.423  8.936
 weights[3] 0.144 0.056  0.075  0.130  0.263  2.662
      alpha 1.321 0.498  0.336  1.340  2.566 22.204
 tail_scale 1.899 0.452  1.308  1.783  3.026  4.808
 tail_shape 0.192 0.147  0.009  0.183  0.469  4.330
  threshold 3.335 0.632  2.335  3.470  4.168  7.863
   shape[1] 1.804 0.193  1.387  1.774  2.088 11.848
   shape[2] 1.680 0.311  1.214  1.655  2.070 10.055
   shape[3] 2.594 0.400  1.874  2.570  3.316 10.182
   scale[1] 1.204 0.392  0.844  1.069  2.357 25.376
   scale[2] 1.812 0.631  0.751  1.874  2.675  5.611
   scale[3] 0.632 0.270  0.453  0.573  1.298  6.873
```

``` r
params_sb_gpd <- params(fit_sb_gpd)
params_sb_gpd
Posterior mean parameters

$alpha
[1] 1.321

$w
[1] 0.4338 0.3600 0.1436

$shape
[1] 1.804 1.680 2.594

$scale
[1] 1.2040 1.8120 0.6318

$tail_scale
[1] 1.899

$tail_shape
[1] 0.1916
```

------------------------------------------------------------------------

### Posterior Predictions

``` r
y_grid <- seq(0, max(y_tail) * 1.3, length.out = 300)
pred_density <- predict(fit_sb_gpd, y = y_grid, type = "density")
plot(pred_density)
```

![](v07-unconditional-DPmixGPD-SB_files/figure-html/density-predict-1.png)

``` r
y_surv <- seq(u_threshold, max(y_tail) * 1.1, length.out = 60)
pred_surv <- predict(fit_sb_gpd, y = y_surv, type = "survival")
plot(pred_surv)
```

![](v07-unconditional-DPmixGPD-SB_files/figure-html/survival-predict-1.png)

``` r
quant_probs <- c(0.90, 0.95, 0.99)
pred_quant <- predict(fit_sb_gpd, type = "quantile", index = quant_probs, interval = "credible")
plot(pred_quant)
```

![](v07-unconditional-DPmixGPD-SB_files/figure-html/quantile-predict-1.png)

------------------------------------------------------------------------

### Tail vs Bulk Comparison

``` r
bundle_sb_bulk <- build_nimble_bundle(
  y = y_tail,
  kernel = "laplace",
  backend = "sb",
  GPD = FALSE,
  components = 5,
  mcmc = list(niter = 2500, nburnin = 500, nchains = 1)
)
fit_sb_bulk <- run_mcmc_bundle_manual(bundle_sb_bulk)
[MCMC] Creating NIMBLE model...
[MCMC] NIMBLE model created successfully.
[MCMC] Configuring MCMC...
===== Monitors =====
thin = 1: alpha, location, scale, w, z
===== Samplers =====
RW sampler (10)
  - alpha
  - location[]  (5 elements)
  - v[]  (4 elements)
conjugate sampler (5)
  - scale[]  (5 elements)
categorical sampler (200)
  - z[]  (200 elements)
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

bulk_quant <- predict(fit_sb_bulk, type = "quantile", index = quant_probs)
t_quant <- predict(fit_sb_gpd, type = "quantile", index = quant_probs)

bind_rows(
  bulk_quant$fit %>% mutate(model = "Bulk-only"),
  t_quant$fit %>% mutate(model = "Bulk + GPD")
) %>%
  select(any_of(c("model", "index", "estimate", "lwr", "upr", "lower", "upper"))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  kable(caption = "Quantiles: Bulk-only vs GPD-augmented", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")
```

|   model    | index | estimate | lower  | upper  |
|:----------:|:-----:|:--------:|:------:|:------:|
| Bulk-only  | 0.90  |  5.892   | 5.100  | 6.919  |
| Bulk-only  | 0.95  |  7.711   | 6.423  | 9.334  |
| Bulk-only  | 0.99  |  12.717  | 10.043 | 16.360 |
| Bulk + GPD | 0.90  |  4.809   | 4.317  | 5.849  |
| Bulk + GPD | 0.95  |  6.432   | 5.673  | 7.991  |
| Bulk + GPD | 0.99  |  10.849  | 9.211  | 14.272 |

Quantiles: Bulk-only vs GPD-augmented

``` r
plot(bulk_quant)
```

![](v07-unconditional-DPmixGPD-SB_files/figure-html/comparison-plots-1.png)

``` r
plot(t_quant)
```

![](v07-unconditional-DPmixGPD-SB_files/figure-html/comparison-plots-2.png)

------------------------------------------------------------------------

### Residuals & Diagnostics

``` r
fit_vals <- fitted(fit_sb_gpd)
plot(fit_vals)
```

![](v07-unconditional-DPmixGPD-SB_files/figure-html/residuals-1.png)![](v07-unconditional-DPmixGPD-SB_files/figure-html/residuals-2.png)

``` r
plot(fit_sb_gpd, family = c("histogram", "autocorrelation", "running"))

=== histogram ===
```

![](v07-unconditional-DPmixGPD-SB_files/figure-html/diagnostics-1.png)

    === autocorrelation ===

![](v07-unconditional-DPmixGPD-SB_files/figure-html/diagnostics-2.png)

    === running ===

![](v07-unconditional-DPmixGPD-SB_files/figure-html/diagnostics-3.png)

------------------------------------------------------------------------

### Key Takeaways

- Stick-breaking truncation fixes the number of bulk components but
  still flexibly models the tail via GPD.
- Posterior [`predict()`](https://rdrr.io/r/stats/predict.html) +
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) workflows
  visualize densities, survival probabilities, and extreme quantiles.
- Comparing bulk-only vs GPD-augmented quantiles reveals how tail
  augmentation shifts the 95–99% levels.
- Next: conditional DPmix (v08–v11) to explore covariate effects before
  moving to causal regimes.
