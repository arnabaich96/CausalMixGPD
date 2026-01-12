---
title: "DQRP Reference"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{DQRP Reference}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
set.seed(4001)
nsim <- 500

# Helper: generate nsim draws from an RNG that effectively supports n = 1
r_vapply <- function(nsim, rng_one, ...) {
  vapply(seq_len(nsim), function(i) rng_one(...), numeric(1))
}

# Helper: keep density() from crashing on non-finite values
keep_finite <- function(x) x[is.finite(x)]

# -------------------------
# Real-line club (R support)
# -------------------------

# Use vapply even for base RNGs, for consistency with your non-vectorized RNGs
s_norm <- r_vapply(nsim, function(mean, sd) rnorm(1, mean = mean, sd = sd), mean = 0, sd = 1)

# Laplace: prefer your package RNG; if not exported, switch to DPmixGPD:::rdexp
s_lap <- r_vapply(
  nsim,
  function(location, scale) rdexp(1, location = location, scale = scale),
  location = 0, scale = 1
)

# Cauchy: base R is rcauchy()
s_cau <- r_vapply(
  nsim,
  function(location, scale) rcauchy(1, location = location, scale = scale),
  location = 0, scale = 1
)

# Normal + GPD (tail-augmented). If your function signature differs, adjust inside rng_one.
s_norm_gpd <- r_vapply(
  nsim,
  function(mean, sd, threshold, tail_scale, tail_shape)
    DPmixGPD::rNormGpd(1,
      mean = mean, sd = sd,
      threshold = threshold,
      tail_scale = tail_scale,
      tail_shape = tail_shape
    ),
  mean = 0, sd = 1,
  threshold = 2.5, tail_scale = 1.2, tail_shape = 0.15
)

# ----------------------------
# Positive club (R+ support)
# ----------------------------

s_gam <- r_vapply(
  nsim,
  function(w, shape, scale)
    DPmixGPD::rGammaMix(1, w = w, shape = shape, scale = scale),
  w = c(0.6, 0.4),
  shape = c(2, 5),
  scale = c(1.2, 0.9)
)

s_lgn <- r_vapply(
  nsim,
  function(w, meanlog, sdlog)
    DPmixGPD::rLognormalMix(1, w = w, meanlog = meanlog, sdlog = sdlog),
  w = c(0.6, 0.4),
  meanlog = c(-0.2, 0.6),
  sdlog = c(0.4, 0.5)
)

s_ig <- r_vapply(
  nsim,
  function(w, mean, shape)
    DPmixGPD::rInvGaussMix(1, w = w, mean = mean, shape = shape),
  w = c(0.6, 0.4),
  mean = c(1.2, 2.0),
  shape = c(3, 5)
)

# Amoroso mix (vignette mode: shape1 fixed at 1)
s_am <- r_vapply(
  nsim,
  function(w, loc, scale, shape1, shape2)
    DPmixGPD::rAmorosoMix(1, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2),
  w = c(0.6, 0.4),
  loc = c(0.1, 0.2),
  scale = c(0.8, 1.2),
  shape1 = c(1, 1),
  shape2 = c(1.1, 1.3)
)

# Gamma mix + GPD tail
s_gam_gpd <- r_vapply(
  nsim,
  function(w, shape, scale, threshold, tail_scale, tail_shape)
    DPmixGPD::rGammaMixGpd(1,
      w = w, shape = shape, scale = scale,
      threshold = threshold,
      tail_scale = tail_scale,
      tail_shape = tail_shape
    ),
  w = c(0.6, 0.4),
  shape = c(2, 5),
  scale = c(1.2, 0.9),
  threshold = 2.5, tail_scale = 1.2, tail_shape = 0.15
)

# -------------------------
# Plot: one panel per club
# -------------------------

op <- par(mfrow = c(1, 2))
on.exit(par(op), add = TRUE)

# Real-line
plot(density(keep_finite(s_norm)), main = "Real-line kernels", xlab = "x")
lines(density(keep_finite(s_lap)), col = "steelblue")
lines(density(keep_finite(s_cau)), col = "firebrick")
lines(density(keep_finite(s_norm_gpd)), col = "darkgreen")
legend("topright",
  legend = c("normal", "laplace", "cauchy", "normal+GPD"),
  col = c("black", "steelblue", "firebrick", "darkgreen"),
  lty = 1, bty = "n"
)
```

![](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\vignettes\dqrp_files/figure-html/dqrp-1.png)<!-- -->

``` r

# Positive
plot(density(keep_finite(s_gam)), main = "Positive kernels", xlab = "x")
lines(density(keep_finite(s_lgn)), col = "steelblue")
lines(density(keep_finite(s_ig)), col = "firebrick")
lines(density(keep_finite(s_am)), col = "darkgreen")
lines(density(keep_finite(s_gam_gpd)), col = "purple")
legend("topright",
  legend = c("gamma", "lognormal", "invgauss", "amoroso", "gamma+GPD"),
  col = c("black", "steelblue", "firebrick", "darkgreen", "purple"),
  lty = 1, bty = "n"
)
```

![](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\vignettes\dqrp_files/figure-html/dqrp-2.png)<!-- -->

``` r

# -------------------------
# Inversion checks (Normal)
# -------------------------

p_vals <- c(0.1, 0.25, 0.5, 0.9)
q_vals <- qnorm(p_vals, mean = 0, sd = 1)
inv_p  <- pnorm(q_vals, mean = 0, sd = 1)
cbind(p = p_vals, p_of_q = inv_p)
#>         p p_of_q
#> [1,] 0.10   0.10
#> [2,] 0.25   0.25
#> [3,] 0.50   0.50
#> [4,] 0.90   0.90

y_vals <- c(-1, 0, 1, 2)
q_of_p <- qnorm(pnorm(y_vals, mean = 0, sd = 1), mean = 0, sd = 1)
cbind(y = y_vals, q_of_p = q_of_p)
#>       y q_of_p
#> [1,] -1     -1
#> [2,]  0      0
#> [3,]  1      1
#> [4,]  2      2
```
