---
title: "Kernels"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Kernels}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Real-line kernels


``` r
data("nc_real200_k2")
print_dataset_header(nc_real200_k2, "nc_real200_k2")
#> 
#> Dataset: nc_real200_k2 
#> head(y):
#> [1] -1.7079605 -0.5634694 -2.2334859  1.8734314  1.4309543 -0.3023361
#> meta: n=200 | support=real | p=0 | K_true=2 | tail=FALSE
```


``` r
real_kernels <- c("normal", "laplace", "cauchy")
backends <- c("crp", "sb")
mcmc <- default_mcmc()

for (ker in real_kernels) {
  for (backend in backends) {
    bundle <- build_nimble_bundle(
      y = nc_real200_k2$y,
      backend = backend,
      kernel = ker,
      GPD = FALSE,
      components = nc_real200_k2$meta$K_true,
      mcmc = mcmc
    )
    summary(bundle)
    fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
    summary(fit)
  }
}
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel        Normal Distribution
#>  Components                          2
#>           N                        200
#>           X                         NO
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                 normal     
#>           meta components info           model                      2     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk       mean dist component (1:2)   normal(mean=0, sd=5)     
#>           bulk         sd dist component (1:2) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 4 
#>   alpha, z[1:200], mean[1:2], sd[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, mean, sd, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (4)
#>   - sd[]  (2 elements)
#>   - mean[]  (2 elements)
#> CRP sampler (1)
#>   - z[1:200]
#>   [Warning] sampler_CRP: The number of clusters based on the cluster parameters
#>             is less than the number of potential clusters. The MCMC is not
#>             strictly valid if it ever proposes more components than cluster
#>             parameters exist; NIMBLE will warn you if this occurs.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#> running chain 2...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#>   [Warning] There are 17 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Normal Distribution
#>  Components                      2
#>           N                    200
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                     sb     
#>           meta     kernel info           model                 normal     
#>           meta components info           model                      2     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk       mean dist component (1:2)   normal(mean=0, sd=5)     
#>           bulk         sd dist component (1:2) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:2], z[1:200], mean[1:2], sd[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
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
#> thin = 1: alpha, mean, sd, w, z
#> ===== Samplers =====
#> RW sampler (2)
#>   - alpha
#>   - v[]  (1 element)
#> conjugate sampler (4)
#>   - mean[]  (2 elements)
#>   - sd[]  (2 elements)
#> categorical sampler (200)
#>   - z[]  (200 elements)
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
#>   [Warning] There are 17 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel       Laplace Distribution
#>  Components                          2
#>           N                        200
#>           X                         NO
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                laplace     
#>           meta components info           model                      2     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk   location dist component (1:2)   normal(mean=0, sd=5)     
#>           bulk      scale dist component (1:2) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 4 
#>   alpha, z[1:200], location[1:2], scale[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, location, scale, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (4)
#>   - scale[]  (2 elements)
#>   - location[]  (2 elements)
#> CRP sampler (1)
#>   - z[1:200]
#>   [Warning] sampler_CRP: The number of clusters based on the cluster parameters
#>             is less than the number of potential clusters. The MCMC is not
#>             strictly valid if it ever proposes more components than cluster
#>             parameters exist; NIMBLE will warn you if this occurs.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#> running chain 2...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#>   [Warning] There are 51 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel   Laplace Distribution
#>  Components                      2
#>           N                    200
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                     sb     
#>           meta     kernel info           model                laplace     
#>           meta components info           model                      2     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk   location dist component (1:2)   normal(mean=0, sd=5)     
#>           bulk      scale dist component (1:2) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:2], z[1:200], location[1:2], scale[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
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
#> thin = 1: alpha, location, scale, w, z
#> ===== Samplers =====
#> RW sampler (4)
#>   - alpha
#>   - location[]  (2 elements)
#>   - v[]  (1 element)
#> conjugate sampler (2)
#>   - scale[]  (2 elements)
#> categorical sampler (200)
#>   - z[]  (200 elements)
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
#>   [Warning] There are 48 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel        Cauchy Distribution
#>  Components                          2
#>           N                        200
#>           X                         NO
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                 cauchy     
#>           meta components info           model                      2     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk   location dist component (1:2)   normal(mean=0, sd=5)     
#>           bulk      scale dist component (1:2) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 4 
#>   alpha, z[1:200], location[1:2], scale[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, location, scale, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (4)
#>   - location[]  (2 elements)
#>   - scale[]  (2 elements)
#> CRP sampler (1)
#>   - z[1:200]
#>   [Warning] sampler_CRP: The number of clusters based on the cluster parameters
#>             is less than the number of potential clusters. The MCMC is not
#>             strictly valid if it ever proposes more components than cluster
#>             parameters exist; NIMBLE will warn you if this occurs.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#> running chain 2...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#>   [Warning] There are 60 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Cauchy Distribution
#>  Components                      2
#>           N                    200
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                     sb     
#>           meta     kernel info           model                 cauchy     
#>           meta components info           model                      2     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk   location dist component (1:2)   normal(mean=0, sd=5)     
#>           bulk      scale dist component (1:2) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:2], z[1:200], location[1:2], scale[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
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
#> thin = 1: alpha, location, scale, w, z
#> ===== Samplers =====
#> RW sampler (6)
#>   - alpha
#>   - location[]  (2 elements)
#>   - scale[]  (2 elements)
#>   - v[]  (1 element)
#> categorical sampler (200)
#>   - z[]  (200 elements)
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
#>   [Warning] There are 60 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
```

## Positive-support kernels


``` r
data("nc_pos200_k3")
print_dataset_header(nc_pos200_k3, "nc_pos200_k3")
#> 
#> Dataset: nc_pos200_k3 
#> head(y):
#> [1] 0.6453111 2.8722793 9.8424691 3.1839504 7.2954021 7.0877631
#> meta: n=200 | support=positive | p=0 | K_true=3 | tail=FALSE
```


``` r
pos_kernels <- c("gamma", "lognormal", "invgauss", "amoroso")
backends <- c("crp", "sb")
mcmc <- default_mcmc()

for (ker in pos_kernels) {
  for (backend in backends) {
    bundle <- build_nimble_bundle(
      y = nc_pos200_k3$y,
      backend = backend,
      kernel = ker,
      GPD = FALSE,
      components = nc_pos200_k3$meta$K_true,
      mcmc = mcmc
    )
    summary(bundle)
    fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
    summary(fit)
  }
}
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel         Gamma Distribution
#>  Components                          3
#>           N                        200
#>           X                         NO
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                  gamma     
#>           meta components info           model                      3     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk      shape dist component (1:3) gamma(shape=2, rate=1)     
#>           bulk      scale dist component (1:3) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 4 
#>   alpha, z[1:200], shape[1:3], scale[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, scale, shape, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (6)
#>   - scale[]  (3 elements)
#>   - shape[]  (3 elements)
#> CRP sampler (1)
#>   - z[1:200]
#>   [Warning] sampler_CRP: The number of clusters based on the cluster parameters
#>             is less than the number of potential clusters. The MCMC is not
#>             strictly valid if it ever proposes more components than cluster
#>             parameters exist; NIMBLE will warn you if this occurs.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#> running chain 2...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#>   [Warning] There are 5 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel     Gamma Distribution
#>  Components                      3
#>           N                    200
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                     sb     
#>           meta     kernel info           model                  gamma     
#>           meta components info           model                      3     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk      shape dist component (1:3) gamma(shape=2, rate=1)     
#>           bulk      scale dist component (1:3) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:3], z[1:200], shape[1:3], scale[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, scale, shape, w, z
#> ===== Samplers =====
#> RW sampler (6)
#>   - alpha
#>   - shape[]  (3 elements)
#>   - v[]  (2 elements)
#> conjugate sampler (3)
#>   - scale[]  (3 elements)
#> categorical sampler (200)
#>   - z[]  (200 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 39 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel     Lognormal Distribution
#>  Components                          3
#>           N                        200
#>           X                         NO
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model              lognormal     
#>           meta components info           model                      3     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk    meanlog dist component (1:3)   normal(mean=0, sd=5)     
#>           bulk      sdlog dist component (1:3) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 4 
#>   alpha, z[1:200], meanlog[1:3], sdlog[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, meanlog, sdlog, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (6)
#>   - sdlog[]  (3 elements)
#>   - meanlog[]  (3 elements)
#> CRP sampler (1)
#>   - z[1:200]
#>   [Warning] sampler_CRP: The number of clusters based on the cluster parameters
#>             is less than the number of potential clusters. The MCMC is not
#>             strictly valid if it ever proposes more components than cluster
#>             parameters exist; NIMBLE will warn you if this occurs.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#> running chain 2...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#>   [Warning] There are 63 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel Lognormal Distribution
#>  Components                      3
#>           N                    200
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                     sb     
#>           meta     kernel info           model              lognormal     
#>           meta components info           model                      3     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk    meanlog dist component (1:3)   normal(mean=0, sd=5)     
#>           bulk      sdlog dist component (1:3) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:3], z[1:200], meanlog[1:3], sdlog[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, meanlog, sdlog, w, z
#> ===== Samplers =====
#> RW sampler (3)
#>   - alpha
#>   - v[]  (2 elements)
#> conjugate sampler (6)
#>   - meanlog[]  (3 elements)
#>   - sdlog[]  (3 elements)
#> categorical sampler (200)
#>   - z[]  (200 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 70 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                         Value
#>     Backend    Chinese Restaurant Process
#>      Kernel Inverse Gaussian Distribution
#>  Components                             3
#>           N                           200
#>           X                            NO
#>         GPD                         FALSE
#>     Epsilon                         0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model               invgauss     
#>           meta components info           model                      3     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk       mean dist component (1:3) gamma(shape=2, rate=1)     
#>           bulk      shape dist component (1:3) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 4 
#>   alpha, z[1:200], mean[1:3], shape[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, mean, shape, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (6)
#>   - mean[]  (3 elements)
#>   - shape[]  (3 elements)
#> CRP sampler (1)
#>   - z[1:200]
#>   [Warning] sampler_CRP: The number of clusters based on the cluster parameters
#>             is less than the number of potential clusters. The MCMC is not
#>             strictly valid if it ever proposes more components than cluster
#>             parameters exist; NIMBLE will warn you if this occurs.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#> running chain 2...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#>   [Warning] There are 21 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                         Value
#>     Backend        Stick-Breaking Process
#>      Kernel Inverse Gaussian Distribution
#>  Components                             3
#>           N                           200
#>           X                            NO
#>         GPD                         FALSE
#>     Epsilon                         0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                     sb     
#>           meta     kernel info           model               invgauss     
#>           meta components info           model                      3     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk       mean dist component (1:3) gamma(shape=2, rate=1)     
#>           bulk      shape dist component (1:3) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:3], z[1:200], mean[1:3], shape[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, mean, shape, w, z
#> ===== Samplers =====
#> RW sampler (9)
#>   - alpha
#>   - mean[]  (3 elements)
#>   - shape[]  (3 elements)
#>   - v[]  (2 elements)
#> categorical sampler (200)
#>   - z[]  (200 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 26 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel       Amoroso Distribution
#>  Components                          3
#>           N                        200
#>           X                         NO
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                amoroso     
#>           meta components info           model                      3     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk        loc dist component (1:3)   normal(mean=0, sd=5)     
#>           bulk      scale dist component (1:3) gamma(shape=2, rate=1)     
#>           bulk     shape1 dist component (1:3) gamma(shape=2, rate=1)     
#>           bulk     shape2 dist component (1:3) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 6 
#>   alpha, z[1:200], loc[1:3], scale[1:3], shape1[1:3], shape2[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, loc, scale, shape1, shape2, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (12)
#>   - loc[]  (3 elements)
#>   - scale[]  (3 elements)
#>   - shape1[]  (3 elements)
#>   - shape2[]  (3 elements)
#> CRP sampler (1)
#>   - z[1:200]
#>   [Warning] sampler_CRP: The number of clusters based on the cluster parameters
#>             is less than the number of potential clusters. The MCMC is not
#>             strictly valid if it ever proposes more components than cluster
#>             parameters exist; NIMBLE will warn you if this occurs.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#> running chain 2...
#>   [Warning] CRP_sampler: This MCMC is not for a proper model. The MCMC attempted to use more components than the number of cluster parameters. Please increase the number of cluster parameters.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel   Amoroso Distribution
#>  Components                      3
#>           N                    200
#>           X                     NO
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                     sb     
#>           meta     kernel info           model                amoroso     
#>           meta components info           model                      3     
#>           meta          N info           model                    200     
#>           meta          P info           model                      0     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk        loc dist component (1:3)   normal(mean=0, sd=5)     
#>           bulk      scale dist component (1:3) gamma(shape=2, rate=1)     
#>           bulk     shape1 dist component (1:3) gamma(shape=2, rate=1)     
#>           bulk     shape2 dist component (1:3) gamma(shape=2, rate=1)     
#>                     notes
#>                          
#>                          
#>                          
#>                          
#>                          
#>  stochastic concentration
#>     iid across components
#>     iid across components
#>     iid across components
#>     iid across components
#> 
#> Monitors
#>   n = 7 
#>   alpha, w[1:3], z[1:200], loc[1:3], scale[1:3], shape1[1:3], shape2[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, loc, scale, shape1, shape2, w, z
#> ===== Samplers =====
#> RW sampler (15)
#>   - alpha
#>   - loc[]  (3 elements)
#>   - scale[]  (3 elements)
#>   - shape1[]  (3 elements)
#>   - shape2[]  (3 elements)
#>   - v[]  (2 elements)
#> categorical sampler (200)
#>   - z[]  (200 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 49 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
```
