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
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)

bundle
DPmixGPD causal bundle
PS model: Bayesian logit (T | X) 
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> Field </th>
   <th style="text-align:center;"> Treated </th>
   <th style="text-align:center;"> Control </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Backend </td>
   <td style="text-align:center;"> Stick-Breaking Process </td>
   <td style="text-align:center;"> Stick-Breaking Process </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Kernel </td>
   <td style="text-align:center;"> normal </td>
   <td style="text-align:center;"> normal </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Components </td>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> GPD tail </td>
   <td style="text-align:center;"> TRUE </td>
   <td style="text-align:center;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Epsilon </td>
   <td style="text-align:center;"> 0.025 </td>
   <td style="text-align:center;"> 0.025 </td>
  </tr>
</tbody>
</table>
Outcome PS included: TRUE 
n (control) = 19 | n (treated) = 13 
```

## MCMC Sampling

``` r

fit <- run_mcmc_causal(bundle, show_progress = FALSE)
```

## Average Treatment Effect (ATE)

``` r
ate_result <- ate(fit, interval = "hpd", nsim_mean = 100)
print(ate_result)
ATE (Average Treatment Effect)
  Prediction points: 32
  Conditional (covariates): YES
  Propensity score used: YES
  PS scale: logit
  Posterior mean draws: 100
  Credible interval: hpd

ATE estimates (treated - control):
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> id </th>
   <th style="text-align:center;"> estimate </th>
   <th style="text-align:center;"> lower </th>
   <th style="text-align:center;"> upper </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 7.097e+66 </td>
   <td style="text-align:center;"> -2.748e+27 </td>
   <td style="text-align:center;"> 7.638e+39 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> 1.093e+67 </td>
   <td style="text-align:center;"> -4.881e+27 </td>
   <td style="text-align:center;"> 7.560e+39 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> 3.148e+57 </td>
   <td style="text-align:center;"> -7.112e+25 </td>
   <td style="text-align:center;"> 5.380e+33 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> 6.369e+67 </td>
   <td style="text-align:center;"> -3.864e+28 </td>
   <td style="text-align:center;"> 7.036e+39 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 5 </td>
   <td style="text-align:center;"> 2.952e+104 </td>
   <td style="text-align:center;"> -1.131e+40 </td>
   <td style="text-align:center;"> 3.774e+63 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> 1.571e+65 </td>
   <td style="text-align:center;"> -1.173e+28 </td>
   <td style="text-align:center;"> 1.351e+38 </td>
  </tr>
</tbody>
</table>... (26 more rows)

X_new <- data.frame(
  wt = seq(min(X$wt), max(X$wt), length.out = 20),
  hp = stats::median(X$hp),
  qsec = stats::median(X$qsec),
  cyl = stats::median(X$cyl)
)
ate_grid <- ate(fit, newdata = X_new, interval = "hpd", nsim_mean = 100, level = 0.90)

print(ate_grid)
ATE (Average Treatment Effect)
  Prediction points: 20
  Conditional (covariates): YES
  Propensity score used: YES
  PS scale: logit
  Posterior mean draws: 100
  Credible interval: hpd

ATE estimates (treated - control):
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> id </th>
   <th style="text-align:center;"> estimate </th>
   <th style="text-align:center;"> lower </th>
   <th style="text-align:center;"> upper </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 3.563e+74 </td>
   <td style="text-align:center;"> -1.061e+31 </td>
   <td style="text-align:center;"> 9.152e+29 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> 3.155e+74 </td>
   <td style="text-align:center;"> -1.105e+31 </td>
   <td style="text-align:center;"> 7.386e+29 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> 4.199e+74 </td>
   <td style="text-align:center;"> -7.024e+30 </td>
   <td style="text-align:center;"> 6.078e+29 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> 4.387e+74 </td>
   <td style="text-align:center;"> -7.787e+30 </td>
   <td style="text-align:center;"> 5.287e+29 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 5 </td>
   <td style="text-align:center;"> 4.332e+74 </td>
   <td style="text-align:center;"> -6.408e+30 </td>
   <td style="text-align:center;"> 4.840e+29 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> 4.792e+74 </td>
   <td style="text-align:center;"> -5.116e+30 </td>
   <td style="text-align:center;"> 3.809e+29 </td>
  </tr>
</tbody>
</table>... (14 more rows)
summary(ate_grid)
ATE Summary
================================================== 
Prediction points: 20
Conditional: YES | PS used: YES
Posterior mean draws: 100
Interval: hpd

Model specification:
  Backend (trt/con): sb / sb
  Kernel (trt/con): normal / normal
  GPD tail (trt/con): YES / YES

ATE statistics:
  Mean: 5.652e+74 | Median: 5.129e+74
  Range: [3.155e+74, 9.235e+74]
  SD: 1.614e+74

Credible interval width:
  Mean: 4.480e+30 | Median: 3.759e+30
  Range: [2.087e+26, 1.179e+31]
```

### ATE Visualization

The `if (interactive()) plot()` method for ATE objects supports multiple
visualization types:

- `type = "both"` (default): Returns a list with `trt_control` and
  `treatment_effect` plots
- `type = "effect"`: Treatment effect curve with credible intervals
- `type = "arms"`: Treated vs control mean outcomes

``` r

ate_plots <- if (interactive()) plot(ate_grid)
ate_plots$treatment_effect
NULL
```

``` r

ate_plots$trt_control
NULL
```

## Quantile Treatment Effect (QTE)

``` r
probs <- c(0.1, 0.5, 0.9)
qte_result <- qte(fit, probs = probs, interval = "hpd")
print(qte_result)
QTE (Quantile Treatment Effect)
  Prediction points: 32
  Quantile grid: 0.1, 0.5, 0.9
  Conditional (covariates): YES
  Propensity score used: YES
  PS scale: logit
  Credible interval: hpd

QTE estimates (treated - control):
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> index </th>
   <th style="text-align:center;"> id </th>
   <th style="text-align:center;"> estimate </th>
   <th style="text-align:center;"> lower </th>
   <th style="text-align:center;"> upper </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 6.504e+65 </td>
   <td style="text-align:center;"> -2.256e+26 </td>
   <td style="text-align:center;"> 5.320e+38 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> 1.037e+66 </td>
   <td style="text-align:center;"> -3.514e+26 </td>
   <td style="text-align:center;"> 5.329e+38 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> 3.981e+56 </td>
   <td style="text-align:center;"> -5.333e+24 </td>
   <td style="text-align:center;"> 4.170e+32 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> 6.539e+66 </td>
   <td style="text-align:center;"> -3.866e+27 </td>
   <td style="text-align:center;"> 4.826e+38 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 5 </td>
   <td style="text-align:center;"> 3.301e+103 </td>
   <td style="text-align:center;"> -1.027e+39 </td>
   <td style="text-align:center;"> 3.287e+62 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> 1.636e+64 </td>
   <td style="text-align:center;"> -7.144e+26 </td>
   <td style="text-align:center;"> 6.866e+36 </td>
  </tr>
</tbody>
</table>... (90 more rows)

qte_grid <- qte(fit, probs = probs, newdata = X_new, interval = "hpd")
print(qte_grid)
QTE (Quantile Treatment Effect)
  Prediction points: 20
  Quantile grid: 0.1, 0.5, 0.9
  Conditional (covariates): YES
  Propensity score used: YES
  PS scale: logit
  Credible interval: hpd

QTE estimates (treated - control):
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> index </th>
   <th style="text-align:center;"> id </th>
   <th style="text-align:center;"> estimate </th>
   <th style="text-align:center;"> lower </th>
   <th style="text-align:center;"> upper </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 3.702e+73 </td>
   <td style="text-align:center;"> -8.451e+29 </td>
   <td style="text-align:center;"> 2.589e+43 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 2 </td>
   <td style="text-align:center;"> 3.881e+73 </td>
   <td style="text-align:center;"> -7.417e+29 </td>
   <td style="text-align:center;"> 2.664e+43 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 3 </td>
   <td style="text-align:center;"> 4.069e+73 </td>
   <td style="text-align:center;"> -6.509e+29 </td>
   <td style="text-align:center;"> 2.742e+43 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 4 </td>
   <td style="text-align:center;"> 4.266e+73 </td>
   <td style="text-align:center;"> -5.712e+29 </td>
   <td style="text-align:center;"> 2.821e+43 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 5 </td>
   <td style="text-align:center;"> 4.472e+73 </td>
   <td style="text-align:center;"> -5.013e+29 </td>
   <td style="text-align:center;"> 2.904e+43 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 6 </td>
   <td style="text-align:center;"> 4.689e+73 </td>
   <td style="text-align:center;"> -4.399e+29 </td>
   <td style="text-align:center;"> 2.988e+43 </td>
  </tr>
</tbody>
</table>... (54 more rows)
summary(qte_grid)
QTE Summary
================================================== 
Prediction points: 20 | Quantiles: 3
Quantile grid: 0.1, 0.5, 0.9
Conditional: YES | PS used: YES
Interval: hpd

Model specification:
  Backend (trt/con): sb / sb
  Kernel (trt/con): normal / normal
  GPD tail (trt/con): YES / YES

QTE by quantile:
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> quantile </th>
   <th style="text-align:center;"> mean_qte </th>
   <th style="text-align:center;"> median_qte </th>
   <th style="text-align:center;"> min_qte </th>
   <th style="text-align:center;"> max_qte </th>
   <th style="text-align:center;"> sd_qte </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 0.1 </td>
   <td style="text-align:center;"> 6.017e+73 </td>
   <td style="text-align:center;"> 5.802e+73 </td>
   <td style="text-align:center;"> 3.702e+73 </td>
   <td style="text-align:center;"> 9.073e+73 </td>
   <td style="text-align:center;"> 1.668e+73 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.5 </td>
   <td style="text-align:center;"> 3.940e+74 </td>
   <td style="text-align:center;"> 3.798e+74 </td>
   <td style="text-align:center;"> 2.423e+74 </td>
   <td style="text-align:center;"> 5.949e+74 </td>
   <td style="text-align:center;"> 1.094e+74 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> 0.9 </td>
   <td style="text-align:center;"> 1.291e+75 </td>
   <td style="text-align:center;"> 1.245e+75 </td>
   <td style="text-align:center;"> 7.943e+74 </td>
   <td style="text-align:center;"> 1.950e+75 </td>
   <td style="text-align:center;"> 3.585e+74 </td>
  </tr>
</tbody>
</table>
Credible interval width:
  Mean: 4.543e+44 | Median: 2.193e+44
  Range: [7.060e+28, 1.538e+45]
```

### QTE Visualization

The `if (interactive()) plot()` method for QTE objects supports multiple
visualization types:

- `type = "both"` (default): Returns a list with `trt_control` and
  `treatment_effect` plots
- `type = "effect"`: Quantile treatment effect curves faceted by
  quantile level
- `type = "arms"`: Treated vs control quantile curves

``` r

qte_plots <- if (interactive()) plot(qte_grid)
qte_plots$treatment_effect
NULL
```

``` r

qte_plots$trt_control
NULL
```

## Model Summary

``` r
summary(fit)
-- PS fit --
DPmixGPD PS fit
model: logit 

-- Outcome fits --
[control]
MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE
n = 19 | components = 6 | epsilon = 0.025
MCMC: niter=400, nburnin=100, thin=2, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.

[treated]
MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE
n = 13 | components = 6 | epsilon = 0.025
MCMC: niter=400, nburnin=100, thin=2, nchains=1 
Fit
Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
```
