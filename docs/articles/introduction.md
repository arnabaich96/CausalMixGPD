# DPmixGPD: quick start

## Goal

This vignette is a fast, end-to-end tour:

- build a model specification,
- run MCMC,
- extract fitted values and predictions.

**Runtime:** designed to run quickly in “FAST” mode.

## What DPmixGPD models

DPmixGPD fits flexible mixture models for the *bulk* of the distribution
and can splice a Generalized Pareto tail beyond a threshold. In
practice, you use it when:

- the center of the data is not well described by a single parametric
  family, and
- the extreme right tail needs principled extrapolation.

## A minimal run

``` r
library(DPmixGPD)

# A toy heavy-ish tail sample
n <- 80
y <- abs(rnorm(n)) + 0.15

bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  components = 6,
  mcmc = mcmc
)

fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
#> [MCMC] Creating NIMBLE model...
#> [MCMC] NIMBLE model created successfully.
#> [MCMC] Configuring MCMC...
#> ===== Monitors =====
#> thin = 1: alpha, mean, sd, tail_scale, tail_shape, threshold, w, z
#> ===== Samplers =====
#> RW sampler (21)
#>   - alpha
#>   - mean[]  (6 elements)
#>   - sd[]  (6 elements)
#>   - threshold
#>   - tail_scale
#>   - tail_shape
#>   - v[]  (5 elements)
#> categorical sampler (80)
#>   - z[]  (80 elements)
#> [MCMC] MCMC configured.
#> [MCMC] Building MCMC object...
#> [MCMC] MCMC object built.
#> [MCMC] Attempting NIMBLE compilation (this may take a minute)...
#> [MCMC] Compiling model...
#> [MCMC] Compiling MCMC sampler...
#> [MCMC] Compilation successful.
#> [MCMC] MCMC execution complete. Processing results...
fit
#> MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE
#> n = 80 | components = 6 | epsilon = 0.025
#> MCMC: niter=400, nburnin=100, thin=2, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
```

## Fitted values and residuals

``` r
f <- fitted(fit, type = "mean", level = 0.90)
head(f)
#>         fit     lower   upper   residuals
#> 1 0.8630258 0.7083039 1.00949 -0.08657197
#> 2 0.8630258 0.7083039 1.00949 -0.52938246
#> 3 0.8630258 0.7083039 1.00949  0.12260283
#> 4 0.8630258 0.7083039 1.00949  0.88225502
#> 5 0.8630258 0.7083039 1.00949 -0.38351801
#> 6 0.8630258 0.7083039 1.00949  0.10744260

# quick residual sanity check
summary(f$residuals)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -0.71192 -0.41112 -0.11071 -0.01264  0.20973  1.68859
```

## Predictions

``` r
# For unconditional models, predict() returns population-level summaries
pred_mean <- predict(fit, type = "mean", cred.level = 0.90, interval = "credible")
pred_q90  <- predict(fit, type = "quantile", index = 0.90, cred.level = 0.90, interval = "credible")

pred_mean$fit
#>    estimate     lower    upper
#> 1 0.8614389 0.7192627 1.001022
pred_q90$fit
#>   estimate index    lower   upper
#> 1 1.620146   0.9 1.228256 1.92826
```

## A quick diagnostic plot

``` r
# Plot methods may vary by version; keep this simple.
# If your plot() method supports a family argument, trace plots are the safest.
try(plot(fit, family = "trace"), silent = TRUE)
```

## Troubleshooting

- **“keywords: if” from NIMBLE**: a covariate column is named `if` (or
  another reserved keyword). Rename columns (e.g., `if` -\> `x_if`).
- **“No space left on device” during build/check**: set
  `TMPDIR`/`TEMP`/`TMP` to a drive with free space (e.g., `D:/Rtmp`).
- **Coverage looks low**: code generation (e.g., `eval(parse())`) is
  hard to trace. Focus tests on user-facing functions.

## Where to go next

- See **Model specification** for all options.
- See **Unconditional** for density and tail diagnostics.
- See **Backends** for SB vs CRP comparisons.
