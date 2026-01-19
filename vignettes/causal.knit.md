---
title: "Causal workflow (two-arm outcome modeling)"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Causal workflow (two-arm outcome modeling)}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Goal

This vignette shows the causal "two-arm" workflow: fit an outcome model separately (or jointly, if your API supports it) for treated and control arms, then compute a treatment effect estimand.

We focus on *distributional* effects, such as the conditional quantile treatment effect (CQTE):

\[
\text{CQTE}(\tau \mid x) = Q_{Y(1)\mid X}(\tau \mid x) - Q_{Y(0)\mid X}(\tau \mid x).
\]

**Important:** the *difference of two densities is not a density* for the treatment effect. If you want the distribution of \(Y(1)-Y(0)\), simulate posterior predictive draws of \(Y(1)\) and \(Y(0)\) and subtract.

## Simulated data


``` r
library(DPmixGPD)

n <- 160
x <- rnorm(n)
X <- data.frame(x = x)

# treatment assignment (no propensity score used here)
a <- rbinom(n, 1, 0.5)

# outcome with heterogeneous effect + heavy-ish noise
te <- 0.4 + 0.6 * (x > 0)
y0 <- 0.5 + 0.7 * x + abs(rnorm(n)) + 0.1
y1 <- y0 + te

y <- ifelse(a == 1, y1, y0)
```

## Fit arm-specific models

The simplest approach is to fit two separate models.


``` r
X0 <- X[a == 0, , drop = FALSE]
X1 <- X[a == 1, , drop = FALSE]
y0_obs <- y[a == 0]
y1_obs <- y[a == 1]

bundle0 <- build_nimble_bundle(
  y = y0_obs,
  X = X0,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

bundle1 <- build_nimble_bundle(
  y = y1_obs,
  X = X1,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit0 <- run_mcmc_bundle_manual(bundle0, show_progress = FALSE)
#> [MCMC] Creating NIMBLE model...
#> [MCMC] NIMBLE model created successfully.
#> [MCMC] Configuring MCMC...
#> ===== Monitors =====
#> thin = 1: alpha, beta_mean, beta_tail_scale, beta_threshold, sd, sdlog_u, tail_shape, threshold, w, z
#> ===== Samplers =====
#> RW sampler (107)
#>   - alpha
#>   - sd[]  (6 elements)
#>   - beta_mean[]  (6 elements)
#>   - sdlog_u
#>   - beta_tail_scale[]  (1 element)
#>   - tail_shape
#>   - v[]  (5 elements)
#>   - threshold[]  (86 elements)
#> conjugate sampler (1)
#>   - beta_threshold[]  (1 element)
#> categorical sampler (86)
#>   - z[]  (86 elements)
#> [MCMC] MCMC configured.
#> [MCMC] Building MCMC object...
#> [MCMC] MCMC object built.
#> [MCMC] Attempting NIMBLE compilation (this may take a minute)...
#> [MCMC] Compiling model...
#> [MCMC] Compiling MCMC sampler...
#> [MCMC] Compilation successful.
#> [MCMC] MCMC execution complete. Processing results...
fit1 <- run_mcmc_bundle_manual(bundle1, show_progress = FALSE)
#> [MCMC] Creating NIMBLE model...
#> [MCMC] NIMBLE model created successfully.
#> [MCMC] Configuring MCMC...
#> ===== Monitors =====
#> thin = 1: alpha, beta_mean, beta_tail_scale, beta_threshold, sd, sdlog_u, tail_shape, threshold, w, z
#> ===== Samplers =====
#> RW sampler (95)
#>   - alpha
#>   - sd[]  (6 elements)
#>   - beta_mean[]  (6 elements)
#>   - sdlog_u
#>   - beta_tail_scale[]  (1 element)
#>   - tail_shape
#>   - v[]  (5 elements)
#>   - threshold[]  (74 elements)
#> conjugate sampler (1)
#>   - beta_threshold[]  (1 element)
#> categorical sampler (74)
#>   - z[]  (74 elements)
#> [MCMC] MCMC configured.
#> [MCMC] Building MCMC object...
#> [MCMC] MCMC object built.
#> [MCMC] Attempting NIMBLE compilation (this may take a minute)...
#> [MCMC] Compiling model...
#> [MCMC] Compiling MCMC sampler...
#> [MCMC] Compilation successful.
#> [MCMC] MCMC execution complete. Processing results...
```

## CQTE on a covariate grid


``` r
new_X <- data.frame(x = seq(min(x), max(x), length.out = 25))

taus <- c(0.1, 0.5, 0.9)

cqte_list <- lapply(taus, function(tau) {
  q0 <- predict(fit0, x = new_X, type = "quantile", index = tau, cred.level = 0.90, interval = "credible")$fit
  q1 <- predict(fit1, x = new_X, type = "quantile", index = tau, cred.level = 0.90, interval = "credible")$fit

  data.frame(
    x = new_X$x,
    tau = tau,
    estimate = q1$estimate - q0$estimate,
    lower = q1$lower - q0$upper,
    upper = q1$upper - q0$lower
  )
})

cqte <- do.call(rbind, cqte_list)
head(cqte)
#>           x tau  estimate     lower    upper
#> 1 -2.214700 0.1 -6.025986 -16.41824 4.885917
#> 2 -2.022353 0.1 -6.077255 -16.03744 4.112770
#> 3 -1.830007 0.1 -6.121980 -15.54986 3.307894
#> 4 -1.637660 0.1 -6.160000 -14.82863 2.480220
#> 5 -1.445314 0.1 -6.190543 -14.13013 1.851430
#> 6 -1.252967 0.1 -6.211886 -13.42793 1.180839
```


``` r
plot(cqte$x[cqte$tau == 0.5], cqte$estimate[cqte$tau == 0.5], type = "l",
     xlab = "x", ylab = "CQTE", main = "Conditional quantile treatment effect")
lines(cqte$x[cqte$tau == 0.1], cqte$estimate[cqte$tau == 0.1])
lines(cqte$x[cqte$tau == 0.9], cqte$estimate[cqte$tau == 0.9])
legend("topleft", legend = paste0("tau=", taus), lty = 1, bty = "n")
```

![](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\docs\articles\causal_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Notes and troubleshooting

- If you later add propensity score adjustment, keep it explicit and documented (it changes assumptions and estimands).
- Arm-specific fits are simple and transparent; a joint causal model can share priors/structure for efficiency.


