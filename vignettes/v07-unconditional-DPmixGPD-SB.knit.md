---
title: "7. Unconditional DPmixGPD with Stick-Breaking Backend"
output: 
  rmarkdown::html_vignette:
    code_folding: show
vignette: >
  %\VignetteIndexEntry{7. Unconditional DPmixGPD SB}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Unconditional DPmixGPD: Stick-Breaking (SB) Backend with Tail

**Goal**: Apply **Stick-Breaking backend** with **GPD tail augmentation** for bulk + tail modeling.

**Model**: 
- **Bulk** ($y \leq u$): Fixed-J stick-breaking mixture
- **Tail** ($y > u$): Generalized Pareto Distribution

---

## Data Setup


``` r
set.seed(42)
bulk_comp1 <- rgamma(80, shape = 2, rate = 1.5)
bulk_comp2 <- rgamma(60, shape = 1, rate = 0.8)
bulk <- c(bulk_comp1, bulk_comp2)

tail_component <- DPmixGPD::rGpd(30, threshold = 0, scale = 1.5, shape = 0.3)
y_tail <- c(bulk, tail_component)

u_threshold <- quantile(y_tail, 0.8)

print("Sample size:", length(y_tail), "\n")
print("Bulk:", sum(y_tail <= u_threshold), "| Tail:", sum(y_tail > u_threshold), "\n")

df_raw <- data.frame(y = y_tail)
p_raw <- ggplot(df_raw, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.6, fill = "darkorange") +
  geom_density(color = "darkred", linewidth = 1) +
  geom_vline(xintercept = u_threshold, linetype = "dashed", color = "gray") +
  labs(title = "Data with Tail", x = "y", y = "Density") +
  theme_minimal()

print(p_raw)
```

---

## Model Specification (SB + GPD)


``` r
spec_sb_gpd <- compile_model_spec(
  y = y_tail,
  kernel = "gamma",
  backend = "sb",           # Stick-Breaking backend
  GPD = TRUE,               # Enable tail
  threshold = u_threshold,
  J = 5,                    # Fixed components
  verbose = FALSE
)

print("SB + GPD Specification:")
print(paste("  Backend (bulk): sb (J =", spec_sb_gpd$J, "components)"))
print(paste("  Tail: GPD (threshold =", u_threshold, ")"))
```

---

## Bundle & MCMC


``` r
bundle_sb_gpd <- build_nimble_bundle(
  spec_sb_gpd,
  mcmc = list(
    niter = 2000,
    nburnin = 500,
    nchains = 2,
    thin = 1
  )
)

fit_sb_gpd <- run_mcmc_bundle_manual(bundle_sb_gpd)

print("\n=== POSTERIOR SUMMARY (SB + GPD) ===\n")
summary(fit_sb_gpd)
```

---

## Posterior Predictions

### Full Distribution


``` r
y_grid <- seq(0, max(y_tail) * 1.3, length.out = 300)

# Posterior predictive density
pred_density <- predict(fit_sb_gpd, y = y_grid, type = "density")

# Use S3 plot method
plot(pred_density)
```

---

## CRP vs SB for GPD Tail


``` r
# CRP + GPD (from vignette 6)
bundle_crp_gpd <- build_nimble_bundle(
  y = y_tail,
  kernel = "gamma",
  backend = "crp",
  GPD = TRUE,
  threshold = u_threshold,
  Kmax = 5,
  mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
)
fit_crp_gpd <- run_mcmc_bundle_manual(bundle_crp_gpd)

print("\n=== CRP vs SB (with GPD Tail) ===\n")
```

---

## Residual Analysis


``` r
residuals_vals <- residuals(fit_sb_gpd)

p_resid <- ggplot(data.frame(r = residuals_vals), aes(x = r)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.6, fill = "mediumpurple") +
  geom_density(color = "purple", linewidth = 1) +
  labs(title = "Residuals: SB + GPD", x = "Residuals", y = "Density") +
  theme_minimal()

print(p_resid)
```

---

## Key Takeaways

- **SB + GPD**: Fixed-component bulk with flexible tail
- **Speed**: Generally faster than CRP + GPD
- **Trade-off**: CRP more flexible; SB more efficient
- **Next**: Conditional models (vignettes 8-11)

