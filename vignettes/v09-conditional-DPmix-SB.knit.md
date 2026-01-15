---
title: "9. Conditional DPmix with Stick-Breaking Backend"
output: 
  rmarkdown::html_vignette:
    code_folding: show
vignette: >
  %\VignetteIndexEntry{9. Conditional DPmix SB}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Conditional DPmix: Stick-Breaking Backend with Covariates

**Goal**: Apply **Stick-Breaking backend** with **covariates** for conditional mixture modeling.

**Model**: Fixed-J stick-breaking mixture conditional on covariates $X$

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
y <- c(component_1, component_2)

df_data <- data.frame(y = y, x1 = X[, 1])
print(ggplot(df_data, aes(x = x1, y = y)) + geom_point() + theme_minimal())
```

---

## Model Specification (Conditional, SB)


``` r
spec_cond_sb <- compile_model_spec(
  y = y,
  X = X,
  kernel = "gamma",
  backend = "sb",           # Stick-Breaking
  GPD = FALSE,
  J = 5,                    # Fixed components
  verbose = FALSE
)

print("Conditional SB Specification:")
print(paste("  Backend: SB (J =", spec_cond_sb$J, ")"))
print(paste("  Covariates:", ncol(X)))
```

---

## Bundle & MCMC


``` r
bundle_cond_sb <- build_nimble_bundle(
  spec_cond_sb,
  mcmc = list(
    niter = 2000,
    nburnin = 500,
    nchains = 2,
    thin = 1
  )
)

fit_cond_sb <- run_mcmc_bundle_manual(bundle_cond_sb)

print("\n=== POSTERIOR SUMMARY (Conditional SB) ===\n")
summary(fit_cond_sb)
```

---

## Conditional Predictions


``` r
X_new <- cbind(x1 = c(-1, 0, 1), x2 = c(-1, 0, 1))
y_grid <- seq(-2, 10, length.out = 100)

# Conditional density predictions for each covariate level
densities_list <- list()
for (i in 1:nrow(X_new)) {
  pred <- predict(fit_cond_sb, x = X_new[i, , drop = FALSE], y = y_grid, type = "density")
  densities_list[[i]] <- data.frame(
    y = pred$fit$y,
    density = pred$fit$density,
    group = paste("X1=", round(X_new[i, 1], 2), sep = "")
  )
}

df_pred <- do.call(rbind, densities_list)

p_pred <- ggplot(df_pred, aes(x = y, y = density, color = group)) +
  geom_line(linewidth = 1) +
  labs(title = "Conditional Density (SB Backend)", x = "y", y = "Density") +
  theme_minimal()

print(p_pred)
```

---

## CRP vs SB (Conditional)


``` r
# CRP conditional (from vignette 8)
bundle_cond_crp <- build_nimble_bundle(
  y = y,
  X = X,
  kernel = "gamma",
  backend = "crp",
  GPD = FALSE,
  Kmax = 5,
  mcmc = list(niter = 1000, nburnin = 200, nchains = 1)
)
fit_cond_crp <- run_mcmc_bundle_manual(bundle_cond_crp)

print("\n=== CRP vs SB (Conditional) ===\n")
```

---

## Key Takeaways

- **SB + Covariates**: Fixed components simplify inference
- **Comparison**: CRP vs SB trade-off applies to conditional models too
- **Next**: GPD augmentation in vignettes 10-11

