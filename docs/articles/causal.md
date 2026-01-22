# Causal Workflow

## Overview

This vignette demonstrates the causal inference workflow using
[`build_causal_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_causal_bundle.md)
and
[`run_mcmc_causal()`](https://arnabaich96.github.io/DPmixGPD/reference/run_mcmc_causal.md).
The package computes distributional treatment effects:

- **Average Treatment Effect (ATE)**: $`E[Y(1) - Y(0) \mid X]`$
- **Quantile Treatment Effect (QTE)**:
  $`Q_{Y(1)}(\tau) - Q_{Y(0)}(\tau)`$

## Theory (brief)

The causal workflow models the potential outcome distributions for
treated and control groups while adjusting for covariates and
(optionally) propensity scores. For each treatment arm \$t \\in
\\{0,1\\}\$, \$\$ f_t(y \\mid x) = \\int K(y; \\theta_t(x))\\,
dG_t(\\theta_t), \$\$ and causal estimands are derived from differences
in the fitted distributions, such as \$\\mathbb{E}\[Y(1) - Y(0) \\mid
X\]\$ and quantile contrasts.

## Data Setup

``` r
library(DPmixGPD)

data("mtcars", package = "datasets")
df <- mtcars
X <- df[, c("wt", "hp", "qsec", "cyl")]
X <- as.data.frame(X)
T_ind <- df$am
y <- df$mpg
```

## Causal Bundle Construction

The
[`build_causal_bundle()`](https://arnabaich96.github.io/DPmixGPD/reference/build_causal_bundle.md)
function creates a unified structure containing:

- Propensity score (PS) model (optional)
- Control arm outcome model
- Treated arm outcome model

``` r
bundle <- build_causal_bundle(
  y = y,
  X = X,
  T = T_ind,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  PS = "logit",
  design = "observational",
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)

bundle
#> DPmixGPD causal bundle
#> PS model: Bayesian logit (T | X) 
#> Outcome (treated): backend = sb | kernel = normal 
#> Outcome (control): backend = sb | kernel = normal 
#> GPD tail (treated/control): TRUE / TRUE 
#> components (treated/control): 6 / 6 
#> Outcome PS included: TRUE 
#> epsilon (treated/control): 0.025 / 0.025 
#> n (control) = 19 | n (treated) = 13
```

## MCMC Sampling

``` r
fit <- run_mcmc_causal(bundle, show_progress = FALSE)
```

## Average Treatment Effect (ATE)

``` r
ate_result <- ate(fit, interval = "hpd", nsim_mean = 100)
print(ate_result)
#> ATE (Average Treatment Effect)
#>   Prediction points: 32
#>   Conditional (covariates): YES
#>   Propensity score used: YES
#>   PS scale: logit
#>   Posterior mean draws: 100
#>   Credible interval: hpd
#> 
#> ATE estimates (treated - control):
#>  id      estimate         lower        upper
#>   1  7.096668e+66 -2.747790e+27 7.637892e+39
#>   2  1.092792e+67 -4.880833e+27 7.560155e+39
#>   3  3.148231e+57 -7.112172e+25 5.379520e+33
#>   4  6.368702e+67 -3.863970e+28 7.036002e+39
#>   5 2.951968e+104 -1.131116e+40 3.774128e+63
#>   6  1.570600e+65 -1.172550e+28 1.350685e+38
#> ... (26 more rows)

X_new <- data.frame(
  wt = seq(min(X$wt), max(X$wt), length.out = 20),
  hp = stats::median(X$hp),
  qsec = stats::median(X$qsec),
  cyl = stats::median(X$cyl)
)
ate_grid <- ate(fit, newdata = X_new, interval = "hpd", nsim_mean = 100, level = 0.90)

print(ate_grid)
#> ATE (Average Treatment Effect)
#>   Prediction points: 20
#>   Conditional (covariates): YES
#>   Propensity score used: YES
#>   PS scale: logit
#>   Posterior mean draws: 100
#>   Credible interval: hpd
#> 
#> ATE estimates (treated - control):
#>  id     estimate         lower        upper
#>   1 3.562911e+74 -1.060663e+31 9.152400e+29
#>   2 3.155030e+74 -1.105315e+31 7.386341e+29
#>   3 4.198869e+74 -7.024421e+30 6.078306e+29
#>   4 4.386679e+74 -7.786960e+30 5.286629e+29
#>   5 4.331852e+74 -6.407762e+30 4.840438e+29
#>   6 4.791778e+74 -5.115586e+30 3.809410e+29
#> ... (14 more rows)
summary(ate_grid)
#> ATE Summary
#> ================================================== 
#> Prediction points: 20
#> Conditional: YES | PS used: YES
#> Posterior mean draws: 100
#> Interval: hpd
#> 
#> Model specification:
#>   Backend (trt/con): sb / sb
#>   Kernel (trt/con): normal / normal
#>   GPD tail (trt/con): YES / YES
#> 
#> ATE statistics:
#>   Mean: 5.65186376147264e+74 | Median: 5.12898071566214e+74
#>   Range: [3.15502974835712e+74, 9.2349665402096e+74]
#>   SD: 1.61418131241249e+74
#> 
#> Credible interval width:
#>   Mean: 4.47971558863383e+30 | Median: 3.75941826993285e+30
#>   Range: [2.08686307002747e+26, 1.17917838120348e+31]
```

### ATE Visualization

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
ATE objects supports multiple visualization types:

- `type = "both"` (default): Returns a list with `trt_control` and
  `treatment_effect` plots
- `type = "effect"`: Treatment effect curve with credible intervals
- `type = "arms"`: Treated vs control mean outcomes

``` r
ate_plots <- plot(ate_grid)
ate_plots$treatment_effect
```

![](causal_files/figure-html/unnamed-chunk-5-1.png)

``` r
ate_plots$trt_control
```

![](causal_files/figure-html/unnamed-chunk-6-1.png)

## Quantile Treatment Effect (QTE)

``` r
probs <- c(0.1, 0.5, 0.9)
qte_result <- qte(fit, probs = probs, interval = "hpd")
print(qte_result)
#> QTE (Quantile Treatment Effect)
#>   Prediction points: 32
#>   Quantile grid: 0.1, 0.5, 0.9
#>   Conditional (covariates): YES
#>   Propensity score used: YES
#>   PS scale: logit
#>   Credible interval: hpd
#> 
#> QTE estimates (treated - control):
#>  index id      estimate         lower        upper
#>    0.1  1  6.504045e+65 -2.256136e+26 5.320135e+38
#>    0.1  2  1.037103e+66 -3.513881e+26 5.328788e+38
#>    0.1  3  3.980819e+56 -5.332544e+24 4.169594e+32
#>    0.1  4  6.539472e+66 -3.865800e+27 4.825881e+38
#>    0.1  5 3.301256e+103 -1.026751e+39 3.286743e+62
#>    0.1  6  1.636091e+64 -7.144390e+26 6.866406e+36
#> ... (90 more rows)

qte_grid <- qte(fit, probs = probs, newdata = X_new, interval = "hpd")
print(qte_grid)
#> QTE (Quantile Treatment Effect)
#>   Prediction points: 20
#>   Quantile grid: 0.1, 0.5, 0.9
#>   Conditional (covariates): YES
#>   Propensity score used: YES
#>   PS scale: logit
#>   Credible interval: hpd
#> 
#> QTE estimates (treated - control):
#>  index id     estimate         lower        upper
#>    0.1  1 3.701822e+73 -8.451444e+29 2.588857e+43
#>    0.1  2 3.881035e+73 -7.416909e+29 2.664172e+43
#>    0.1  3 4.068925e+73 -6.509011e+29 2.741678e+43
#>    0.1  4 4.265911e+73 -5.712248e+29 2.821439e+43
#>    0.1  5 4.472434e+73 -5.013017e+29 2.903521e+43
#>    0.1  6 4.688955e+73 -4.399377e+29 2.987990e+43
#> ... (54 more rows)
summary(qte_grid)
#> QTE Summary
#> ================================================== 
#> Prediction points: 20 | Quantiles: 3
#> Quantile grid: 0.1, 0.5, 0.9
#> Conditional: YES | PS used: YES
#> Interval: hpd
#> 
#> Model specification:
#>   Backend (trt/con): sb / sb
#>   Kernel (trt/con): normal / normal
#>   GPD tail (trt/con): YES / YES
#> 
#> QTE by quantile:
#>  quantile     mean_qte   median_qte      min_qte      max_qte       sd_qte
#>       0.1 6.017011e+73 5.802194e+73 3.701822e+73 9.073018e+73 1.668230e+73
#>       0.5 3.939864e+74 3.798390e+74 2.423385e+74 5.948655e+74 1.093560e+74
#>       0.9 1.291351e+75 1.244946e+75 7.942781e+74 1.950085e+75 3.584829e+74
#> 
#> Credible interval width:
#>   Mean: 4.54299494313147e+44 | Median: 2.19277425358873e+44
#>   Range: [7.05972174744674e+28, 1.53846697783865e+45]
```

### QTE Visualization

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
QTE objects supports multiple visualization types:

- `type = "both"` (default): Returns a list with `trt_control` and
  `treatment_effect` plots
- `type = "effect"`: Quantile treatment effect curves faceted by
  quantile level
- `type = "arms"`: Treated vs control quantile curves

``` r
qte_plots <- plot(qte_grid)
qte_plots$treatment_effect
```

![](causal_files/figure-html/unnamed-chunk-8-1.png)

``` r
qte_plots$trt_control
```

![](causal_files/figure-html/unnamed-chunk-9-1.png)

## Model Summary

``` r
summary(fit)
#> -- PS fit --
#> DPmixGPD PS fit
#> model: logit 
#> 
#> -- Outcome fits --
#> [control]
#> MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE
#> n = 19 | components = 6 | epsilon = 0.025
#> MCMC: niter=400, nburnin=100, thin=2, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
#> 
#> [treated]
#> MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE
#> n = 13 | components = 6 | epsilon = 0.025
#> MCMC: niter=400, nburnin=100, thin=2, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
```
