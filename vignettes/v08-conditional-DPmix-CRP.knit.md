---
title: "8. Conditional DPmix with CRP Backend"
output: 
  rmarkdown::html_vignette:
    code_folding: show
vignette: >
  %\VignetteIndexEntry{8. Conditional DPmix CRP}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Conditional DPmix: CRP Backend with Covariates

**Goal**: Model outcome $y$ conditional on **covariates** $X$ using nonparametric DP mixture.

**Model**: $y_i | X_i, G_i \sim \int K(y_i; \theta_i) dG_i(\theta_i)$ where $\theta_i$ depends on $X_i$

**Backend**: CRP for conditional mixture

---

## Data Setup


``` r
set.seed(42)
n <- 150

# Covariates
X <- cbind(
  x1 = rnorm(n, mean = 0, sd = 1),
  x2 = rnorm(n, mean = 0, sd = 1)
)

# Outcome: mixture conditional on X
mean_shift_1 <- 1 + 0.5 * X[, 1] + 0.3 * X[, 2]
mean_shift_2 <- 3 + 0.2 * X[, 1] - 0.4 * X[, 2]

component_1 <- rgamma(75, shape = 2, rate = 1.5) + mean_shift_1[1:75]
component_2 <- rgamma(75, shape = 1, rate = 0.8) + mean_shift_2[76:150]
y <- c(component_1, component_2)

print("Sample size:", n, "\n")
print("Covariates:", ncol(X), "\n")

# Visualization
df_data <- data.frame(y = y, x1 = X[, 1], x2 = X[, 2])

p_scatter <- ggplot(df_data, aes(x = x1, y = y)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", fill = NA) +
  labs(title = "Outcome vs Covariate X1", x = "X1", y = "y") +
  theme_minimal()

print(p_scatter)
```

---

## Model Specification (Conditional, CRP)


``` r
spec_cond <- compile_model_spec(
  y = y,
  X = X,                     # NEW: Include covariates
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  Kmax = 5,
  verbose = FALSE
)

print("Conditional Specification:")
print(paste("  Outcome (y):", length(y), "observations"))
print(paste("  Covariates (X):", nrow(X), "×", ncol(X)))
print("  Kernel: gamma (conditional)")
print("  Backend: CRP\n")
```

---

## Bundle & MCMC


``` r
bundle_cond <- build_nimble_bundle(
  spec_cond,
  mcmc = list(
    niter = 2000,
    nburnin = 500,
    nchains = 2,
    thin = 1
  )
)

fit_cond <- run_mcmc_bundle_manual(bundle_cond)

print("\n=== POSTERIOR SUMMARY (Conditional, CRP) ===\n")
summary(fit_cond)
```

---

## Conditional Predictions

### Predict at Specific Covariate Values


``` r
# New covariate values for prediction
X_new <- cbind(
  x1 = c(-1, 0, 1),
  x2 = c(-1, 0, 1)
)

# Predict for each covariate combination
y_grid <- seq(-2, 10, length.out = 100)

# For each row of X_new, get conditional density
densities_list <- list()
for (i in 1:nrow(X_new)) {
  X_rep <- X_new[i, , drop = FALSE]
  dens <- predict(fit_cond, newdata = y_grid, X = X_rep)
  densities_list[[i]] <- data.frame(
    y = y_grid,
    density = dens,
    group = paste("X1=", X_new[i, 1], ", X2=", X_new[i, 2], sep = "")
  )
}

df_cond_pred <- do.call(rbind, densities_list)

p_cond_pred <- ggplot(df_cond_pred, aes(x = y, y = density, color = group)) +
  geom_line(linewidth = 1) +
  labs(title = "Conditional Predictive Density", x = "y", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_cond_pred)
```

---

## Effect of Covariates on Outcome Distribution


``` r
# Isolate effect of X1 (holding X2 = 0)
X1_range <- seq(-2, 2, by = 0.5)
X2_fixed <- 0

for (x1_val in X1_range[c(1, 3, 5)]) {  # Show 3 examples
  X_eval <- cbind(x1 = x1_val, x2 = X2_fixed)
  y_grid <- seq(-1, 10, length.out = 100)
  dens <- predict(fit_cond, newdata = y_grid, X = X_eval)
  
  print("X1 =", x1_val, "=> Posterior predictive mean:", weighted.mean(y_grid, dens), "\n")
}
```

---

## Covariate Coefficients


``` r
# Extract covariate effects from posterior
coefs <- coef(fit_cond)
print("Covariate coefficients (posterior mean):\n")
print(head(coefs))
```

---

## Heteroscedasticity Analysis


``` r
# Extract fitted values with diagnostics
fit_vals <- fitted(fit_cond)

# Use S3 plot method for diagnostic plots
plot(fit_vals)
```

---

## Unconditional vs Conditional


``` r
# Fit unconditional model on same data
bundle_uncond <- build_nimble_bundle(
  y = y,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  Kmax = 5,
  mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
)
fit_uncond <- run_mcmc_bundle_manual(bundle_uncond)

print("\n=== MODEL COMPARISON ===\n")
```

---

## Key Takeaways

- **Conditional Models**: Account for covariate relationships
- **Heteroscedasticity**: DP mixture captures varying conditional distributions
- **Model Selection**: Conditional usually better with informative covariates
- **Next**: Stick-Breaking conditional in vignette 9

