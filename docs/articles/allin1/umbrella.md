# Umbrella: Custom Models (All-in-One)

## Overview

This vignette collects two end-to-end examples with customized priors
and links:

1.  **Conditional CRP + Amoroso + GPD** with covariates, custom links,
    and tail settings.  
2.  **Causal model (two arms)** with SB backends: Normal (treated) and
    Laplace (control), with covariate-linked locations and custom
    priors.

Both examples use `mtcars` and show the main **user-level functions**
plus **S3 methods** (`print`, `summary`, `plot`, `predict`, `fitted`,
`residuals`, `params`, `ate`, `qte`).

> Note on link priors: both SB and CRP backends currently require
> **normal** priors for link-mode regression coefficients. The requested
> **lognormal** prior for the scale link is therefore shown below as a
> **non-executed** spec block.

## Data

``` r

library(DPmixGPD)

data("mtcars", package = "datasets")
df <- mtcars
y <- df$mpg
X <- df[, c("wt", "hp")]
X <- as.data.frame(X)
```

## Model 1: CRP + Amoroso + GPD (Conditional)

### Specification

- **Backend**: CRP  
- **Kernel**: Amoroso  
- **Covariates**: `wt`, `hp`  
- **Bulk**:
  - `loc`: identity link, normal prior on coefficients
  - `scale`: identity link, normal prior on coefficients (CRP
    restriction)
  - `shape1`: fixed at 1
  - `shape2`: default prior
- **Tail**:
  - `threshold`: exp link, normal prior on coefficients
  - `tail_scale`: exp link (default prior)

``` r

param_specs_amoroso <- list(
  bulk = list(
    loc = list(
      mode = "link",
      link = "identity",
      beta_prior = list(dist = "normal", args = list(mean = 0, sd = 2))
    ),
    scale = list(
      mode = "link",
      link = "identity",
      beta_prior = list(dist = "normal", args = list(mean = 0, sd = 2))
    ),
    shape1 = list(mode = "fixed", value = 1)
  ),
  gpd = list(
    threshold = list(
      mode = "link",
      link = "exp",
      beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.2))
    ),
    tail_scale = list(
      mode = "link",
      link = "exp"
    )
  )
)
```

``` r

# Requested variant (not runnable): link-mode beta priors must be normal
param_specs_amoroso_requested <- list(
  bulk = list(
    loc = list(
      mode = "link",
      link = "identity",
      beta_prior = list(dist = "normal", args = list(mean = 0, sd = 2))
    ),
    scale = list(
      mode = "link",
      link = "identity",
      beta_prior = list(dist = "lognormal", args = list(meanlog = 0, sdlog = 1))
    ),
    shape1 = list(mode = "fixed", value = 1)
  ),
  gpd = list(
    threshold = list(
      mode = "link",
      link = "exp",
      beta_prior = list(dist = "normal", args = list(mean = 0, sd = 0.2))
    ),
    tail_scale = list(
      mode = "link",
      link = "exp"
    )
  )
)
```

### Build + Run (CRP note)

NIMBLE’s CRP sampler cannot cluster **deterministic** nodes created by
link-mode parameters. As a result, CRP + covariate links are not
runnable with the current backend. We still record the **requested CRP
specification** below (for reference), and then run an **SB-equivalent**
model that supports the same customization and lets this vignette
compile end-to-end.

``` r

# Requested CRP specification (not runnable due to CRP link-mode restriction)
bundle_amoroso_crp <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "crp",
  kernel = "amoroso",
  GPD = TRUE,
  components = 5,
  param_specs = param_specs_amoroso,
  mcmc = mcmc
)
```

``` r

# Runnable SB equivalent (supports link-mode parameters)
bundle_amoroso <- build_nimble_bundle(
  y = y,
  X = X,
  backend = "sb",
  kernel = "amoroso",
  GPD = TRUE,
  components = 5,
  param_specs = param_specs_amoroso,
  mcmc = mcmc
)

fit_amoroso <- run_mcmc_bundle_manual(bundle_amoroso, show_progress = FALSE)
```

### User-Level Functions

``` r

print(bundle_amoroso)
#> DPmixGPD bundle
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel   Amoroso Distribution
#>  Components                      5
#>           N                     32
#>           X              YES (P=2)
#>         GPD                   TRUE
#>     Epsilon                  0.025
#> 
#>   contains  : code, constants, data, dimensions, inits, monitors
summary(bundle_amoroso)
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel   Amoroso Distribution
#>  Components                      5
#>           N                     32
#>           X              YES (P=2)
#>         GPD                   TRUE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter  mode             level
#>           meta    backend  info             model
#>           meta     kernel  info             model
#>           meta components  info             model
#>           meta          N  info             model
#>           meta          P  info             model
#>  concentration      alpha  dist            scalar
#>           bulk        loc  link        regression
#>           bulk      scale  link        regression
#>           bulk     shape1 fixed   component (1:5)
#>           bulk     shape2  dist   component (1:5)
#>            gpd  threshold  link observation (1:N)
#>            gpd tail_scale  link observation (1:N)
#>            gpd tail_shape  dist            scalar
#>                                     prior     link
#>                                        sb         
#>                                   amoroso         
#>                                         5         
#>                                        32         
#>                                         2         
#>                    gamma(shape=1, rate=1)         
#>           beta_loc ~ normal(mean=0, sd=2) identity
#>         beta_scale ~ normal(mean=0, sd=2) identity
#>                                         1         
#>                    gamma(shape=2, rate=1)         
#>   beta_threshold ~ normal(mean=0, sd=0.2)      exp
#>  beta_tail_scale ~ normal(mean=0, sd=0.5)      exp
#>                    normal(mean=0, sd=0.2)         
#>                                                       notes
#>                                                            
#>                                                            
#>                                                            
#>                                                            
#>                                                            
#>                                    stochastic concentration
#>                          beta_loc is 5 x 2 (components x P)
#>                        beta_scale is 5 x 2 (components x P)
#>                                                            
#>                                       iid across components
#>                                beta_threshold is length P=2
#>  beta_tail_scale is length P=2; tail_scale[i] deterministic
#>                                                            
#> 
#> Monitors
#>   n = 11 
#>   alpha, w[1:5], z[1:32], beta_loc[1:5,1:2], beta_scale[1:5,1:2], shape1[1:5], shape2[1:5], threshold[1:32], beta_threshold[1:2], beta_tail_scale[1:2], tail_shape
```

``` r

print(fit_amoroso)
#> MixGPD fit | backend: Stick-Breaking Process | kernel: Amoroso Distribution | GPD tail: TRUE
#> n = 32 | components = 5 | epsilon = 0.025
#> MCMC: niter=600, nburnin=150, thin=2, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
summary(fit_amoroso)
#> MixGPD summary | backend: Stick-Breaking Process | kernel: Amoroso Distribution | GPD tail: TRUE | epsilon: 0.025
#> n = 32 | components = 5
#> Summary
#> Initial components: 5 | Components after truncation: 1
#> 
#> WAIC: 289.883
#> lppd: -143.126 | pWAIC: 1.816
#> 
#> Summary table
#>           parameter   mean    sd q0.025 q0.500 q0.975     ess
#>          weights[1]  0.887 0.162    0.5      1      1   8.127
#>               alpha  0.571 0.398  0.083  0.479  1.405   5.822
#>      beta_loc[1, 1]  0.233 2.174 -3.944  0.256  4.638  93.618
#>      beta_loc[2, 1] -0.014 1.928 -3.591  -0.05  3.526  87.763
#>      beta_loc[3, 1] -0.348 2.164 -4.435 -0.446  3.641   80.98
#>      beta_loc[4, 1]  0.405 2.072 -3.342  0.292  3.974 111.196
#>      beta_loc[5, 1] -0.134 1.923 -3.755 -0.215  3.335 185.091
#>      beta_loc[1, 2]   1.05 1.191 -1.062  0.903  3.649  34.465
#>      beta_loc[2, 2]  0.622 1.659 -2.142  0.507  4.138  76.334
#>      beta_loc[3, 2]  0.451 1.844 -3.082  0.677  3.426  63.533
#>      beta_loc[4, 2] -0.063 1.904 -3.762  0.133  3.416  93.672
#>      beta_loc[5, 2]  0.601 1.904 -3.487  0.612  4.384 136.447
#>    beta_scale[1, 1]  0.216 1.842 -3.195 -0.029  4.315 118.789
#>    beta_scale[2, 1] -0.236 2.016 -4.741 -0.147  3.515  99.498
#>    beta_scale[3, 1] -0.263 2.004 -4.147 -0.399  4.233  72.565
#>    beta_scale[4, 1] -0.384 2.089 -4.604 -0.269  3.131  90.501
#>    beta_scale[5, 1]  0.137 1.913 -3.345  0.196  4.024  71.448
#>    beta_scale[1, 2]  1.287 1.703 -2.587  1.228  5.205  62.151
#>    beta_scale[2, 2]  0.766 1.756 -2.976  0.833  3.866  48.742
#>    beta_scale[3, 2]  0.328 2.243 -4.384  0.703  4.556   58.72
#>    beta_scale[4, 2]  0.033 1.864 -3.245  0.111  3.689   98.23
#>    beta_scale[5, 2] -0.185 1.938 -3.569 -0.354  3.827  108.64
#>  beta_tail_scale[1]  0.874 0.093  0.683   0.87  1.037  15.682
#>  beta_tail_scale[2]     -0 0.001 -0.002      0  0.002   2.349
#>   beta_threshold[1]  0.098 0.222 -0.269  0.068  0.431  10.477
#>   beta_threshold[2] -0.076 0.125 -0.388      0  0.008  11.704
#>          tail_shape  0.313 0.131  0.071  0.318  0.566   67.33
#>           shape1[1]      1     0      1      1      1       0
#>           shape2[1]   2.73 1.891  0.162  2.226  7.515  24.347
```

### S3 Methods (Fit)

``` r

tryCatch(
  plot(fit_amoroso, family = "trace"),
  error = function(e) {
    message("Plotting 'fit_amoroso' with family = 'trace' failed: ", conditionMessage(e))
  }
)
```

``` r

pred_mean <- predict(fit_amoroso, x = X, type = "mean", cred.level = 0.90, interval = "credible")
pred_q90 <- predict(fit_amoroso, x = X, type = "quantile", index = 0.90, cred.level = 0.90, interval = "credible")
head(pred_mean$fit)
#>   estimate   lower upper
#> 1    10.44  2.1136  31.5
#> 2    13.19  4.3880  35.3
#> 3     7.13  0.9482  25.9
#> 4    19.71  9.6634  47.5
#> 5    21.76 -0.0165  57.1
#> 6    25.74  6.8658  52.5
head(pred_q90$fit)
#>   estimate index id lower upper
#> 1     33.6   0.9  1  24.8  48.1
#> 2     42.1   0.9  2  29.5  61.0
#> 3     25.9   0.9  3  19.7  36.8
#> 4     57.0   0.9  4  37.8  83.4
#> 5     68.9   0.9  5  43.9 104.6
#> 6     70.9   0.9  6  44.6 107.0
```

``` r

fvals <- fitted(fit_amoroso, type = "mean", level = 0.90)
head(fvals)
#>     fit lower upper residuals
#> 1  9.77 2.624  31.5     11.23
#> 2 13.56 0.917  38.5      7.44
#> 3  7.16 0.364  26.1     15.64
#> 4 19.59 7.485  46.2      1.81
#> 5 21.78 1.763  60.3     -3.08
#> 6 25.20 9.714  57.3     -7.10
summary(fvals$residuals)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -153.006  -10.219   -0.635  -10.438   11.268   29.688
```

``` r

raw_resid <- residuals(fit_amoroso, type = "raw")
head(raw_resid)
#> [1] 11.23  7.44 15.64  1.81 -3.08 -7.10
```

``` r

p <- params(fit_amoroso)
names(p)
#> [1] "alpha"           "w"               "beta_loc"        "beta_scale"     
#> [5] "shape1"          "shape2"          "beta_threshold"  "beta_tail_scale"
#> [9] "tail_shape"
```

## Model 2: Causal (SB Normal vs SB Laplace)

### Specification

- **Backend**: SB (both arms)  
- **Treated arm (T=1)**: Normal kernel
  - Location: identity link, Gaussian prior  
  - SD: Inverse-Gamma prior  
- **Control arm (T=0)**: Laplace kernel
  - Location: identity link, Gaussian prior  
  - Scale: Gamma prior

``` r

X_causal <- df[, c("wt", "hp", "qsec", "cyl")]
X_causal <- as.data.frame(X_causal)
T_ind <- df$am

param_specs_causal <- list(
  trt = list(
    bulk = list(
      mean = list(
        mode = "link",
        link = "identity",
        beta_prior = list(dist = "normal", args = list(mean = 0, sd = 2))
      ),
      sd = list(
        mode = "dist",
        dist = "invgamma",
        args = list(shape = 2, scale = 1)
      )
    )
  ),
  con = list(
    bulk = list(
      location = list(
        mode = "link",
        link = "identity",
        beta_prior = list(dist = "normal", args = list(mean = 0, sd = 2))
      ),
      scale = list(
        mode = "dist",
        dist = "gamma",
        args = list(shape = 2, rate = 1)
      )
    )
  )
)
```

### Build + Run

``` r

causal_bundle <- build_causal_bundle(
  y = y,
  X = X_causal,
  T = T_ind,
  backend = "sb",
  kernel = c("normal", "laplace"),
  GPD = FALSE,
  components = 5,
  param_specs = param_specs_causal,
  PS = "logit",
  design = "observational",
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)

causal_fit <- run_mcmc_causal(causal_bundle, show_progress = FALSE)
```

### User-Level Functions

``` r

print(causal_bundle)
#> DPmixGPD causal bundle
#> PS model: Bayesian logit (T | X) 
#> Outcome (treated): backend = sb | kernel = normal 
#> Outcome (control): backend = sb | kernel = laplace 
#> GPD tail (treated/control): FALSE / FALSE 
#> components (treated/control): 5 / 5 
#> Outcome PS included: TRUE 
#> epsilon (treated/control): 0.025 / 0.025 
#> n (control) = 19 | n (treated) = 13
summary(causal_bundle)
#> DPmixGPD causal bundle summary
#> DPmixGPD causal bundle
#> PS model: Bayesian logit (T | X) 
#> Outcome (treated): backend = sb | kernel = normal 
#> Outcome (control): backend = sb | kernel = laplace 
#> GPD tail (treated/control): FALSE / FALSE 
#> components (treated/control): 5 / 5 
#> Outcome PS included: TRUE 
#> epsilon (treated/control): 0.025 / 0.025 
#> n (control) = 19 | n (treated) = 13
```

``` r

print(causal_fit)
#> DPmixGPD causal fit
#> PS model: Bayesian logit (T | X) 
#> Outcome (treated): backend = sb | kernel = normal 
#> Outcome (control): backend = sb | kernel = laplace 
#> GPD tail (treated/control): FALSE / FALSE
summary(causal_fit)
#> -- PS fit --
#> DPmixGPD PS fit
#> model: logit 
#> 
#> -- Outcome fits --
#> [control]
#> MixGPD fit | backend: Stick-Breaking Process | kernel: Laplace Distribution | GPD tail: FALSE
#> n = 19 | components = 5 | epsilon = 0.025
#> MCMC: niter=600, nburnin=150, thin=2, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
#> 
#> [treated]
#> MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: FALSE
#> n = 13 | components = 5 | epsilon = 0.025
#> MCMC: niter=600, nburnin=150, thin=2, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
```

### S3 Methods (Causal)

``` r

try(plot(causal_fit, family = "trace"), silent = TRUE)
```

``` r

pred_causal_mean <- predict(causal_fit, x = X_causal, type = "mean", interval = "credible")
head(pred_causal_mean)
#>                      ps estimate lower upper
#> Mazda RX4         0.510    10.28  -421   454
#> Mazda RX4 Wag     0.367    10.10  -422   455
#> Datsun 710        0.667     1.32  -366   376
#> Hornet 4 Drive    0.253     7.38  -427   452
#> Hornet Sportabout 0.270    29.57  -645   736
#> Valiant           0.162     5.47  -411   430
```

``` r

ate_result <- ate(causal_fit, interval = "credible", nsim_mean = 50)
print(ate_result)
#> ATE (Average Treatment Effect)
#>   Prediction points: 32
#>   Conditional (covariates): YES
#>   Propensity score used: YES
#>   PS scale: logit
#>   Posterior mean draws: 50
#>   Credible interval: credible (95%)
#> 
#> ATE estimates (treated - control):
#>  id estimate    lower   upper
#>   1   10.281 -422.106 455.518
#>   2   10.103 -423.415 455.855
#>   3    1.352 -368.406 378.089
#>   4    7.376 -429.805 453.906
#>   5   29.573 -645.528 737.107
#>   6    5.461 -414.346 432.494
#> ... (26 more rows)
summary(ate_result)
#> ATE Summary
#> ================================================== 
#> Prediction points: 32
#> Conditional: YES | PS used: YES
#> Posterior mean draws: 50
#> Interval: credible (95%)
#> 
#> Model specification:
#>   Backend (trt/con): sb / sb
#>   Kernel (trt/con): normal / laplace
#>   GPD tail (trt/con): NO / NO
#> 
#> ATE statistics:
#>   Mean: 19.688 | Median: 12.723
#>   Range: [-11.387, 77.007]
#>   SD: 22.425
#> 
#> Credible interval width:
#>   Mean: 1164.747 | Median: 983.302
#>   Range: [441.91, 2619.232]
```

``` r

qte_result <- qte(causal_fit, probs = c(0.25, 0.5, 0.75), interval = "credible")
# Plotting may fail on some graphics devices (e.g., non-interactive or CI environments),
# so errors are intentionally suppressed to ensure the vignette renders successfully.
try({
  plot(ate_result, type = "effect")
  plot(qte_result, type = "effect")
}, silent = TRUE)
```

![](umbrella_files/figure-html/unnamed-chunk-20-1.png)
