# 8. Unconditional DPmixGPD with CRP Backend

> **Legacy vignette (for the website / historical notes).** These files
> may not match the current exported API one-to-one. Last verified:
> **2026-01-18**.
>
> For the up-to-date workflow, see the main package vignettes
> (Introduction, Model Spec, MCMC Workflow,
> Unconditional/Conditional/Causal, Backends, S3 Reference).

## Unconditional DPmixGPD: CRP Backend with Tail Augmentation

**Purpose**: Model the **bulk** of the distribution with a DP mixture
(the “kernel” or bulk family) and let a **GPD tail** capture extreme
values beyond a threshold. The CRP backend handles partitioning and we
toggle `GPD = TRUE` to augment the tail.

------------------------------------------------------------------------

### Data Setup

``` r

# Load data with an obvious tail
data("nc_pos_tail200_k4")
y_tail <- nc_pos_tail200_k4$y
y_tail <- y_tail[is.finite(y_tail) & y_tail > 0]

# Summaries and table
summary_tbl <- tibble(
  statistic = c("N", "Mean", "SD", "Min", "Max"),
  value = c(length(y_tail), mean(y_tail), sd(y_tail), min(y_tail), max(y_tail))
)

df_data <- data.frame(y = y_tail)

ggplot(df_data, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "steelblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Raw Data: Mix of Bulk and Tail", x = "y", y = "Density") +
  theme_minimal()
```

![](articles/workflows/legacy-cache/figure-htmldata-setup-1.png)

| statistic |  value  |
|:---------:|:-------:|
|     N     | 200.000 |
|   Mean    |  2.334  |
|    SD     |  2.300  |
|    Min    |  0.328  |
|    Max    | 19.870  |

Summary of the Tail Dataset {.table .table .table-striped .table-hover
style="width: auto !important; margin-left: auto; margin-right: auto;"}

------------------------------------------------------------------------

### Threshold & Tail Partition

``` r

thresholds <- quantile(y_tail, c(0.70, 0.75, 0.80, 0.85, 0.90))
u_threshold <- thresholds["80%"]

df_tail <- tibble(
  y = y_tail,
  region = ifelse(y_tail > u_threshold, "Tail", "Bulk")
)

ggplot(df_tail, aes(x = y, fill = region)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.7, color = "black") +
  geom_vline(xintercept = u_threshold, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Bulk" = "steelblue", "Tail" = "red")) +
  labs(title = "Threshold Partition", x = "y", y = "Density") +
  theme_minimal()
```

![](articles/workflows/legacy-cache/figure-htmlthreshold-selection-1.png)

------------------------------------------------------------------------

### Model Specification & Bundle

This mirrors the direct
[`build_nimble_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_nimble_bundle.md)
workflow in `v04`, but with **tail augmentation** turned on
(`GPD = TRUE`). Here we use an **Inverse Gaussian** bulk kernel and
estimate the GPD threshold with a lognormal prior centered at the
empirical 80th percentile.

``` r

bundle_gpd <- build_nimble_bundle(
  y = y_tail,
  kernel = "invgauss",
  backend = "crp",
  GPD = TRUE,
  components = 5,
  alpha_random = TRUE,
  param_specs = list(
    gpd = list(
      threshold = list(
        mode = "dist",
        dist = "lognormal",
        args = list(meanlog = log(max(u_threshold, .Machine$double.eps)), sdlog = 0.25)
      )
    )
  ),
  mcmc = mcmc
)
```

------------------------------------------------------------------------

### Running MCMC & Diagnostics

``` r

fit_gpd <- load_or_fit("v08-unconditional-DPmixGPD-CRP-fit_gpd", run_mcmc_bundle_manual(bundle_gpd))
```

``` r

summary(fit_gpd)
```

    MixGPD summary | backend: Chinese Restaurant Process | kernel: Inverse Gaussian Distribution | GPD tail: TRUE | epsilon: 0.025
    n = 200 | components = 5
    Summary
    Initial components: 5 | Components after truncation: 1

    WAIC: 661.827
    lppd: -313.537 | pWAIC: 17.377

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
       <td style="text-align:center;"> 0.871 </td>
       <td style="text-align:center;"> 0.177 </td>
       <td style="text-align:center;"> 0.475 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 1 </td>
       <td style="text-align:center;"> 48.788 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> alpha </td>
       <td style="text-align:center;"> 0.367 </td>
       <td style="text-align:center;"> 0.332 </td>
       <td style="text-align:center;"> 0.01 </td>
       <td style="text-align:center;"> 0.27 </td>
       <td style="text-align:center;"> 1.187 </td>
       <td style="text-align:center;"> 448.767 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> tail_scale </td>
       <td style="text-align:center;"> 2.218 </td>
       <td style="text-align:center;"> 1.009 </td>
       <td style="text-align:center;"> 1.168 </td>
       <td style="text-align:center;"> 1.944 </td>
       <td style="text-align:center;"> 5.145 </td>
       <td style="text-align:center;"> 123.022 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> tail_shape </td>
       <td style="text-align:center;"> 0.184 </td>
       <td style="text-align:center;"> 0.15 </td>
       <td style="text-align:center;"> -0.129 </td>
       <td style="text-align:center;"> 0.18 </td>
       <td style="text-align:center;"> 0.468 </td>
       <td style="text-align:center;"> 312.526 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> threshold </td>
       <td style="text-align:center;"> 3.706 </td>
       <td style="text-align:center;"> 1.126 </td>
       <td style="text-align:center;"> 1.782 </td>
       <td style="text-align:center;"> 3.548 </td>
       <td style="text-align:center;"> 6.036 </td>
       <td style="text-align:center;"> 164.736 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> mean[1] </td>
       <td style="text-align:center;"> 2.409 </td>
       <td style="text-align:center;"> 0.602 </td>
       <td style="text-align:center;"> 1.365 </td>
       <td style="text-align:center;"> 2.305 </td>
       <td style="text-align:center;"> 4.067 </td>
       <td style="text-align:center;"> 199.902 </td>
      </tr>
      <tr>
       <td style="text-align:center;"> shape[1] </td>
       <td style="text-align:center;"> 3.358 </td>
       <td style="text-align:center;"> 0.74 </td>
       <td style="text-align:center;"> 2.468 </td>
       <td style="text-align:center;"> 3.234 </td>
       <td style="text-align:center;"> 5.301 </td>
       <td style="text-align:center;"> 346.964 </td>
      </tr>
    </tbody>
    </table>

``` r

params_gpd <- params(fit_gpd)
params_gpd
```

    Posterior mean parameters

    $alpha
    [1] "0.367"

    $w
    [1] "0.871"

    $mean
    [1] "2.409"

    $shape
    [1] "3.358"

    $tail_scale
    [1] "2.218"

    $tail_shape
    [1] "0.184"

``` r

# S3 plot methods highlight trace + density diagnostics
if (interactive()) plot(fit_gpd, family = c("traceplot", "density", "running"))
if (interactive()) plot(fit_gpd, params = "alpha", family = c("traceplot", "autocorrelation", "geweke"))
```

------------------------------------------------------------------------

### Posterior Predictions: Bulk and Tail

We use the **S3 [`predict()`](https://rdrr.io/r/stats/predict.html)
objects** and their dedicated `if (interactive()) plot()` methods to
visualize densities, survival curves, and posterior-mean quantiles
(i.e., averages of q(p \| θ) over draws).

``` r

y_min <- max(min(y_tail), .Machine$double.eps)
y_grid <- seq(y_min, max(y_tail) * 1.3, length.out = 300)
pred_density <- predict(fit_gpd, y = y_grid, type = "density")
if (interactive()) plot(pred_density)
```

``` r

y_surv <- seq(max(u_threshold, y_min), max(y_tail) * 1.1, length.out = 60)
pred_surv <- predict(fit_gpd, y = y_surv, type = "survival")
if (interactive()) plot(pred_surv)
```

``` r

quantile_probs <- c(0.90, 0.95, 0.99)
pred_quantiles <- predict(fit_gpd, type = "quantile", index = quantile_probs, interval = "credible")
if (interactive()) plot(pred_quantiles)
```

------------------------------------------------------------------------

### Bulk-Only Baseline Comparison

To demonstrate the value of tail augmentation, we fit a bulk-only
lognormal DP mixture and compare posterior summaries against the
InvGauss+GPD fit.

``` r

bundle_bulk_only <- build_nimble_bundle(
  y = y_tail,
  kernel = "lognormal",
  backend = "crp",
  GPD = FALSE,
  components = 5,
  mcmc = mcmc
)
fit_bulk_only <- load_or_fit("v08-unconditional-DPmixGPD-CRP-fit_bulk_only", run_mcmc_bundle_manual(bundle_bulk_only))
```

``` r

bulk_quantiles <- predict(fit_bulk_only, type = "quantile", index = quantile_probs)
tail_quantiles <- predict(fit_gpd, type = "quantile", index = quantile_probs)

compare_tbl <- bind_rows(
  bulk_quantiles$fit %>% mutate(model = "Bulk only"),
  tail_quantiles$fit %>% mutate(model = "Bulk + GPD")
) %>%
  select(any_of(c("model", "index", "estimate", "lwr", "upr", "lower", "upper")))

compare_tbl %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  kable(caption = "Posterior-Mean Quantiles for Bulk-Only vs GPD Models", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")
```

|   model    | index | estimate | lower |  upper   |
|:----------:|:-----:|:--------:|:-----:|:--------:|
| Bulk only  | 0.90  | 7.14e+02 | 11.73 | 1.78e+03 |
| Bulk only  | 0.95  | 2.10e+04 | 20.35 | 1.44e+04 |
| Bulk only  | 0.99  | 1.97e+07 | 55.76 | 7.47e+05 |
| Bulk + GPD | 0.90  | 4.73e+00 | 2.30  | 6.23e+00 |
| Bulk + GPD | 0.95  | 6.32e+00 | 2.78  | 8.45e+00 |
| Bulk + GPD | 0.99  | 1.14e+01 | 5.20  | 1.71e+01 |

Posterior-Mean Quantiles for Bulk-Only vs GPD Models {.table .table
.table-striped .table-hover
style="width: auto !important; margin-left: auto; margin-right: auto;"}

``` r

if (interactive()) plot(bulk_quantiles)
if (interactive()) plot(tail_quantiles)
```

------------------------------------------------------------------------

### Extreme Value Diagnostics & Residuals

``` r

probs <- c(0.995, 0.99, 0.975)
return_levels <- predict(fit_gpd, type = "quantile", index = probs)

return_levels$fit %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  kable(caption = "Extreme Quantile Estimates (Posterior Mean and Credible Intervals)", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")
```

| estimate | index | lower | upper |
|:--------:|:-----:|:-----:|:-----:|
|  14.11   | 0.995 | 6.96  | 22.3  |
|  11.35   | 0.990 | 5.20  | 17.1  |
|   8.29   | 0.975 | 3.50  | 11.7  |

Extreme Quantile Estimates (Posterior Mean and Credible Intervals)
{.table .table .table-striped .table-hover
style="width: auto !important; margin-left: auto; margin-right: auto;"}

``` r

fit_vals <- fitted(fit_gpd)
if (interactive()) plot(fit_vals)
```

------------------------------------------------------------------------

### Threshold Sensitivity

``` r

sensitivity_fits <- lapply(thresholds, function(u) {
  bundle <- build_nimble_bundle(
    y = y_tail,
    kernel = "invgauss",
    backend = "crp",
    GPD = TRUE,
    param_specs = list(
      gpd = list(
        threshold = list(mode = "fixed", value = u)
      )
    ),
    components = 5,
    mcmc = mcmc
  )
  fit <- load_or_fit("v08-unconditional-DPmixGPD-CRP-fit", run_mcmc_bundle_manual(bundle))
  list(threshold = u, fit = fit)
})

sensitivity_tbl <- bind_rows(lapply(sensitivity_fits, function(entry) {
  quant <- predict(entry$fit, type = "quantile", index = 0.99, interval = "credible")
  qfit <- as.data.frame(quant$fit)
  qfit$threshold <- entry$threshold

  # Keep the usual columns if present
  keep <- intersect(names(qfit), c("threshold", "index", "estimate", "lower", "upper", "lwr", "upr"))
  qfit <- qfit[, keep, drop = FALSE]

  # Standardize to one set of CI column names
  if (!"lower" %in% names(qfit) && "lwr" %in% names(qfit)) qfit$lower <- qfit$lwr
  if (!"upper" %in% names(qfit) && "upr" %in% names(qfit)) qfit$upper <- qfit$upr

  # If estimate isn't present for some reason, fall back to the first numeric column
  if (!"estimate" %in% names(qfit)) {
    num_cols <- names(qfit)[vapply(qfit, is.numeric, logical(1))]
    if (length(num_cols) > 0) qfit$estimate <- qfit[[num_cols[1]]]
  }

  out <- data.frame(
    threshold = qfit$threshold[1],
    q_99 = qfit$estimate[1],
    q_99_lwr = if ("lower" %in% names(qfit)) qfit$lower[1] else NA_real_,
    q_99_upr = if ("upper" %in% names(qfit)) qfit$upper[1] else NA_real_
  )
  tibble::as_tibble(out)
}))

sensitivity_tbl %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  kable(caption = "Threshold Sensitivity: 99th Quantile", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "center")
```

| threshold | q_99 | q_99_lwr | q_99_upr |
|:---------:|:----:|:--------:|:--------:|
|   2.65    | 11.3 |   7.07   |   16.4   |
|   3.04    | 11.3 |   7.07   |   16.4   |
|   3.51    | 11.3 |   7.07   |   16.4   |
|   3.94    | 11.3 |   7.07   |   16.4   |
|   4.60    | 11.3 |   7.07   |   16.4   |

Threshold Sensitivity: 99th Quantile {.table .table .table-striped
.table-hover
style="width: auto !important; margin-left: auto; margin-right: auto;"}

------------------------------------------------------------------------

### Key Takeaways

- **Bulk + Tail**: DPmix explains the bulk, and GPD captures the
  extremes.
- **Threshold**: 0.8 quantile is our running default, but sensitivity
  checks guide the choice.
- **S3 workflows**: [`predict()`](https://rdrr.io/r/stats/predict.html)
  and its `if (interactive()) plot()` method make it easy to compare
  density, survival, and quantiles.
- **Comparison**: Bulk-only fits under-estimate extreme quantiles
  without a GPD component.
- **Next**: Explore Stick-Breaking (`v07`) for another backend with tail
  augmentation.
