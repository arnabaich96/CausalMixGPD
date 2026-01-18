# Summarize a MixGPD fitted object

Summarize a MixGPD fitted object

## Usage

``` r
# S3 method for class 'mixgpd_fit'
summary(object, pars = NULL, probs = c(0.025, 0.5, 0.975), ...)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- pars:

  Optional character vector of parameters to summarize. If NULL,
  summarize all (excluding v's).

- probs:

  Numeric vector of quantiles to report.

- ...:

  Unused.

## Value

An object of class `"mixgpd_summary"`.

## Examples

``` r
# \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
#> [MCMC] Creating NIMBLE model...
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> [MCMC] NIMBLE model created successfully.
#> [MCMC] Configuring MCMC...
#> ===== Monitors =====
#> thin = 1: alpha, mean, sd, tail_scale, tail_shape, threshold, w, z
#> ===== Samplers =====
#> categorical sampler (50)
#>   - z[]  (50 elements)
#> RW sampler (21)
#>   - alpha
#>   - mean[]  (6 elements)
#>   - sd[]  (6 elements)
#>   - threshold
#>   - tail_scale
#>   - tail_shape
#>   - v[]  (5 elements)
#> [MCMC] MCMC configured.
#> [MCMC] Building MCMC object...
#> [MCMC] MCMC object built.
#> [MCMC] Attempting NIMBLE compilation (this may take a minute)...
#> [MCMC] Compiling model...
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> [MCMC] Compiling MCMC sampler...
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> [MCMC] Compilation successful.
#>   [Warning] To calculate WAIC, set 'WAIC = TRUE', in addition to having enabled WAIC in building the MCMC.
#> running chain 1...
#> |-------------|-------------|-------------|-------------|
#> |-------------------------------------------------------|
#> [MCMC] MCMC execution complete. Processing results...
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Calculating WAIC.
summary(fit, pars = c("alpha", "threshold"))
#> MixGPD summary | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: TRUE | epsilon: 0.025
#> n = 50 | components = 6
#> Summary
#> Initial components: 6 | Components after truncation: 2
#> 
#> WAIC: 76.018
#> lppd: -35.537 | pWAIC: 2.472
#> 
#> Summary table
#>  parameter  mean    sd q0.025 q0.500 q0.975    ess
#>      alpha 0.654 0.293  0.233  0.606  1.298 37.340
#>  threshold 0.384 0.087  0.254  0.463  0.463  1.361
# }
```
