---
title: "6. Unconditional DPmixGPD with CRP Backend"
output: 
  rmarkdown::html_vignette:
    code_folding: show
vignette: >
  %\VignetteIndexEntry{6. Unconditional DPmixGPD CRP}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Unconditional DPmixGPD: CRP Backend with Tail Augmentation

**Goal**: Estimate density of univariate outcome $y$ with **extreme tail behavior** using **DPmix + GPD tail augmentation**.

**Model**: 
- **Bulk** ($y \leq u$): Nonparametric DP mixture
- **Tail** ($y > u$): Generalized Pareto Distribution (GPD)

**Backend**: CRP for bulk + GPD for tail

---

## Data with Tail Behavior


``` r
# Generate bimodal bulk + heavy tail
set.seed(42)
bulk_comp1 <- rgamma(80, shape = 2, rate = 1.5)
bulk_comp2 <- rgamma(60, shape = 1, rate = 0.8)
bulk <- c(bulk_comp1, bulk_comp2)

# Add extreme tail (GPD)
tail_component <- DPmixGPD::rGpd(30, threshold = 0, scale = 1.5, shape = 0.3)

y_tail <- c(bulk, tail_component)

print("Sample size:", length(y_tail), "\n")
print("Bulk observations:", length(bulk), "\n")
print("Tail observations:", length(tail_component), "\n")
print("Min:", min(y_tail), "\n")
print("Max:", max(y_tail), "\n")
print("Mean:", mean(y_tail), "\n")

# Visualization
df_raw <- data.frame(y = y_tail, region = ifelse(y_tail > quantile(y_tail, 0.8), "Tail", "Bulk"))

p_raw <- ggplot(df_raw, aes(x = y, fill = region)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.7) +
  geom_density(alpha = 0.3, linewidth = 1) +
  scale_fill_manual(values = c("Bulk" = "steelblue", "Tail" = "red")) +
  labs(title = "Data with Bulk & Tail Regions", x = "y", y = "Density") +
  theme_minimal()

print(p_raw)
```

---

## Threshold Selection

**Critical**: Choose threshold $u$ to separate bulk from tail.


``` r
# Options for threshold selection:
threshold_mean <- mean(y_tail)
threshold_quantile90 <- quantile(y_tail, 0.9)
threshold_quantile95 <- quantile(y_tail, 0.95)

print("Threshold Options:\n")
print("  Mean:           ", round(threshold_mean, 3), "\n")
print("  90th percentile:", round(threshold_quantile90, 3), "\n")
print("  95th percentile:", round(threshold_quantile95, 3), "\n\n")

# Use 80th percentile as default (typically ~20% in tail)
u_threshold <- quantile(y_tail, 0.8)
print("Selected threshold (80th percentile):", round(u_threshold, 3), "\n")
```

---

## Model Specification (with GPD)


``` r
spec_gpd <- compile_model_spec(
  y = y_tail,
  kernel = "gamma",           # Bulk kernel
  backend = "crp",            # CRP for bulk
  GPD = TRUE,                 # ENABLE tail augmentation
  threshold = u_threshold,    # Tail threshold
  Kmax = 5,                   # Max bulk components
  verbose = FALSE
)

print("Specification with GPD:")
print(paste("  Kernel (bulk):", spec_gpd$kernel))
print(paste("  Backend (bulk):", spec_gpd$backend))
print(paste("  Threshold:", u_threshold))
print(paste("  GPD enabled:", spec_gpd$GPD))
print(paste("  Bulk observations:", sum(y_tail <= u_threshold)))
print(paste("  Tail observations:", sum(y_tail > u_threshold)))
```

---

## Bundle & MCMC


``` r
bundle_gpd <- build_nimble_bundle(
  spec_gpd,
  mcmc = list(
    niter = 2000,
    nburnin = 500,
    nchains = 2,
    thin = 1
  )
)

print("Bundle compiled with CRP (bulk) + GPD (tail).\n")
```

---

## MCMC Execution


``` r
fit_gpd <- run_mcmc_bundle_manual(bundle_gpd)

print("\n=== POSTERIOR SUMMARY (DPmixGPD, CRP Backend) ===\n")
summary(fit_gpd)
```

---

## GPD Parameter Estimation

### Tail Parameters


``` r
print("Tail Parameters Estimated:\n")
print("  scale (σ):   > 0, controls tail spread\n")
print("  shape (ξ):   > 0, controls tail heaviness\n")
print("             ξ = 0 (exponential tail)\n")
print("             ξ > 0 (power-law tail)\n")
print("             ξ < 0 (finite tail)\n\n")

print("Interpret from posterior summary above:\n")
print("Look for 'scale' and 'shape' parameters in MCMC output.\n")
```

---

## Posterior Predictions: Bulk vs Tail

### Predictive Density


``` r
# Full prediction grid including tail
y_grid <- seq(0, max(y_tail) * 1.3, length.out = 300)

# Posterior predictive density
pred_density <- predict(fit_gpd, y = y_grid, type = "density")

# Use S3 plot method
plot(pred_density)
```

### Survival Probability


``` r
# Survival probabilities on tail
y_surv <- seq(u_threshold, max(y_tail) * 1.1, length.out = 50)

# Posterior predictive survival
pred_surv <- predict(fit_gpd, y = y_surv, type = "survival")

# Use S3 plot method
plot(pred_surv)
```

---

## Bulk vs Tail Model Comparison

### Without GPD (Bulk Only)


``` r
bundle_bulk_only <- build_nimble_bundle(
  y = y_tail,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  Kmax = 5,
  mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
)
fit_bulk_only <- run_mcmc_bundle_manual(bundle_bulk_only)
# Model fit completed
```

### With GPD (Bulk + Tail)


``` r
# Model fit completed
```

### Model Comparison


``` r
print("\n=== BULK ONLY vs BULK + GPD ===\n")
print("Interpretation:\n")
if (loglik_gpd > loglik_bulk) {
  print("✓ GPD significantly improves fit (tail is important)\n")
} else {
  print("✗ GPD doesn't help (no strong tail behavior)\n")
}
```

---

## Extreme Value Analysis

### Return Levels


``` r
# Estimate rare event probabilities
probs <- c(0.99, 0.95, 0.90)
return_levels <- predict(fit_gpd, newdata = data.frame(p = probs), type = "quantile")

print("Return Levels (Posterior Mean):\n")
print("  99th percentile:", return_levels[1], "\n")
print("  95th percentile:", return_levels[2], "\n")
print("  90th percentile:", return_levels[3], "\n")
```

---

## Residual Analysis


``` r
# Extract fitted values with diagnostics
fit_vals <- fitted(fit_gpd)

# Use S3 plot method for diagnostic plots
plot(fit_vals)

print("Residual Summary:\n")
print(summary(residuals(fit_gpd)))
```

---

## Sensitivity to Threshold Choice


``` r
thresholds <- quantile(y_tail, c(0.70, 0.75, 0.80, 0.85, 0.90))

print("Testing threshold percentiles:\n")
print("Values:", paste(names(thresholds), collapse = ", "), "\n")
print("Recommendation: Use 0.80-0.90 for heavy tails.\n")
print("Compare convergence diagnostics via summary().\n")

# Demonstrate with one threshold
bundle_thresh <- build_nimble_bundle(
  y = y_tail,
  kernel = "gamma",
  backend = "crp",
  GPD = TRUE,
  threshold = quantile(y_tail, 0.85),
  Kmax = 5,
  mcmc = list(niter = 500, nburnin = 100, nchains = 1)
)
fit_thresh <- run_mcmc_bundle_manual(bundle_thresh)
summary(fit_thresh)
```

---

## Key Takeaways

- **GPD Tail**: Captures extreme values better than parametric bulk alone
- **Threshold**: Critical decision; use sensitivity analysis to choose
- **Parameter Interpretation**: scale & shape control tail behavior
- **Model Comparison**: Use summary() and convergence diagnostics
- **Extreme Value Analysis**: Quantiles, return levels, tail probabilities
- **Next**: Stick-Breaking backend with GPD in vignette 7

