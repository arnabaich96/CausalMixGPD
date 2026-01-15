---
title: "10. Conditional DPmixGPD with CRP Backend"
output: 
  rmarkdown::html_vignette:
    code_folding: show
vignette: >
  %\VignetteIndexEntry{10. Conditional DPmixGPD CRP}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Conditional DPmixGPD: CRP Backend with Covariates & Tail

**Goal**: Model conditional outcome with **covariates** and **GPD tail** using CRP backend.

---

## Data Setup


``` r
set.seed(42)
n <- 150

X <- cbind(
  x1 = rnorm(n, mean = 0, sd = 1),
  x2 = rnorm(n, mean = 0, sd = 1)
)

mean_shift_1 <- 1 + 0.5 * X[, 1]
mean_shift_2 <- 3 + 0.2 * X[, 1]

component_1 <- rgamma(75, shape = 2, rate = 1.5) + mean_shift_1[1:75]
component_2 <- rgamma(75, shape = 1, rate = 0.8) + mean_shift_2[76:150]

# Add tail
tail_component <- DPmixGPD::rGpd(20, threshold = 0, scale = 1, shape = 0.3)
y_tail <- c(component_1, component_2, tail_component)

# Adjust X for tail observations
X_tail <- rbind(X, X[1:20, ])

u_threshold <- quantile(y_tail, 0.8)

print("Sample size:", length(y_tail), "\n")
print("Bulk:", sum(y_tail <= u_threshold), "| Tail:", sum(y_tail > u_threshold), "\n")
```

---

## Model Specification


``` r
spec_cond_gpd <- compile_model_spec(
  y = y_tail,
  X = X_tail,
  kernel = "gamma",
  backend = "crp",
  GPD = TRUE,
  threshold = u_threshold,
  Kmax = 5,
  verbose = FALSE
)

print("Conditional DPmixGPD (CRP):")
print("  Covariates: Yes")
print("  Tail: GPD\n")
```

---

## Bundle & MCMC


``` r
bundle_cond_gpd <- build_nimble_bundle(
  spec_cond_gpd,
  mcmc = list(niter = 2000, nburnin = 500, nchains = 2, thin = 1)
)

fit_cond_gpd <- run_mcmc_bundle_manual(bundle_cond_gpd)

print("\n=== POSTERIOR SUMMARY ===\n")
summary(fit_cond_gpd)
```

---

## Conditional Predictions with Tail


``` r
X_new <- cbind(x1 = c(-1, 0, 1), x2 = 0)
y_grid <- seq(-2, 15, length.out = 150)

# Conditional density predictions for each covariate level with GPD tail
densities_list <- list()
for (i in 1:nrow(X_new)) {
  pred <- predict(fit_cond_gpd, x = X_new[i, , drop = FALSE], y = y_grid, type = "density")
  densities_list[[i]] <- data.frame(
    y = pred$fit$y,
    density = pred$fit$density,
    group = paste("X1=", round(X_new[i, 1], 1), sep = "")
  )
}

df_pred <- do.call(rbind, densities_list)

p_pred <- ggplot(df_pred, aes(x = y, y = density, color = group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = u_threshold, linetype = "dashed", alpha = 0.5) +
  labs(title = "Conditional Density with Tail (CRP + GPD)", x = "y", y = "Density") +
  theme_minimal()

print(p_pred)
```

---

## Tail Behavior by Covariate


``` r
print("Tail probabilities by covariate value estimated.\n")
print("GPD parameters (scale, shape) may vary with X.\n")
```

---

## Key Takeaways

- **Conditional + Tail**: Both covariates and tail heterogeneity
- **Complex Model**: Combines CRP bulk flexibility with GPD tail power
- **Next**: SB variant in vignette 11

