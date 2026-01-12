---
title: "Causal: Same vs Different Kernels"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Causal: Same vs Different Kernels}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
data("causal_pos500_p3_k2")
data("causal_alt_pos500_p3_k3")
data("causal_alt_real500_p4_k2")
```


``` r
mcmc <- default_mcmc()

# Same kernel by arm
cb_same <- build_causal_bundle(
  y = causal_pos500_p3_k2$y,
  X = causal_pos500_p3_k2$X,
  T = causal_pos500_p3_k2$T,
  backend = c("sb", "sb"),
  kernel = c("gamma", "gamma"),
  GPD = c(FALSE, FALSE),
  J = c(causal_pos500_p3_k2$meta$K0, causal_pos500_p3_k2$meta$K1),
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)
cf_same <- run_mcmc_causal(cb_same, show_progress = FALSE)
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: beta
#> ===== Samplers =====
#> RW sampler (4)
#>   - beta[]  (4 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#> Defining model
#> Building model
#> Setting data and initial values
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_ps_scale, beta_scale, shape, w, z
#> ===== Samplers =====
#> RW sampler (12)
#>   - alpha
#>   - shape[]  (2 elements)
#>   - beta_scale[]  (6 elements)
#>   - beta_ps_scale[]  (2 elements)
#>   - v[]  (1 element)
#> categorical sampler (191)
#>   - z[]  (191 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#> running chain 2...
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#>   [Warning] There are 3 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> Defining model
#> Building model
#> Setting data and initial values
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_ps_scale, beta_scale, shape, w, z
#> ===== Samplers =====
#> RW sampler (12)
#>   - alpha
#>   - shape[]  (2 elements)
#>   - beta_scale[]  (6 elements)
#>   - beta_ps_scale[]  (2 elements)
#>   - v[]  (1 element)
#> categorical sampler (309)
#>   - z[]  (309 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#> running chain 2...
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#>   [Warning] There are 25 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.

# Different kernels by arm (positive)
cb_alt_pos <- build_causal_bundle(
  y = causal_alt_pos500_p3_k3$y,
  X = causal_alt_pos500_p3_k3$X,
  T = causal_alt_pos500_p3_k3$T,
  backend = c("sb", "sb"),
  kernel = c("lognormal", "gamma"),
  GPD = c(FALSE, FALSE),
  J = c(causal_alt_pos500_p3_k3$meta$K0, causal_alt_pos500_p3_k3$meta$K1),
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)
cf_alt_pos <- run_mcmc_causal(cb_alt_pos, show_progress = FALSE)
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: beta
#> ===== Samplers =====
#> RW sampler (4)
#>   - beta[]  (4 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_ps_scale, beta_scale, shape, w, z
#> ===== Samplers =====
#> RW sampler (18)
#>   - alpha
#>   - shape[]  (3 elements)
#>   - beta_scale[]  (9 elements)
#>   - beta_ps_scale[]  (3 elements)
#>   - v[]  (2 elements)
#> categorical sampler (225)
#>   - z[]  (225 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 27 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_meanlog, beta_ps_meanlog, sdlog, w, z
#> ===== Samplers =====
#> RW sampler (15)
#>   - alpha
#>   - beta_meanlog[]  (9 elements)
#>   - beta_ps_meanlog[]  (3 elements)
#>   - v[]  (2 elements)
#> conjugate sampler (3)
#>   - sdlog[]  (3 elements)
#> categorical sampler (275)
#>   - z[]  (275 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 45 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.

# Different kernels by arm (real)
cb_alt_real <- build_causal_bundle(
  y = causal_alt_real500_p4_k2$y,
  X = causal_alt_real500_p4_k2$X,
  T = causal_alt_real500_p4_k2$T,
  backend = c("sb", "sb"),
  kernel = c("normal", "laplace"),
  GPD = c(FALSE, FALSE),
  J = c(causal_alt_real500_p4_k2$meta$K0, causal_alt_real500_p4_k2$meta$K1),
  mcmc_outcome = mcmc,
  mcmc_ps = mcmc
)
cf_alt_real <- run_mcmc_causal(cb_alt_real, show_progress = FALSE)
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: beta
#> ===== Samplers =====
#> RW sampler (5)
#>   - beta[]  (5 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#> Defining model
#> Building model
#> Setting data and initial values
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_location, beta_ps_location, scale, w, z
#> ===== Samplers =====
#> RW sampler (12)
#>   - alpha
#>   - beta_location[]  (8 elements)
#>   - beta_ps_location[]  (2 elements)
#>   - v[]  (1 element)
#> conjugate sampler (2)
#>   - scale[]  (2 elements)
#> categorical sampler (232)
#>   - z[]  (232 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#> running chain 2...
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#>   [Warning] There are 104 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> Defining model
#> Building model
#> Setting data and initial values
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_mean, beta_ps_mean, sd, w, z
#> ===== Samplers =====
#> RW sampler (12)
#>   - alpha
#>   - beta_mean[]  (8 elements)
#>   - beta_ps_mean[]  (2 elements)
#>   - v[]  (1 element)
#> conjugate sampler (2)
#>   - sd[]  (2 elements)
#> categorical sampler (268)
#>   - z[]  (268 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#> running chain 2...
#>   [Warning] Incorrect size or dimension of initial value for 'v'.
#>          Initial value will not be used in compiled model.
#>   [Warning] There are 20 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
```
