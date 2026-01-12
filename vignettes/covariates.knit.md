---
title: "Covariates"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Covariates}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
data("nc_posX100_p3_k2")
data("nc_posX100_p4_k3")
data("nc_posX100_p5_k4")
```


``` r
data("nc_realX100_p3_k2")
data("nc_realX100_p5_k3")
```


``` r
pos_sets <- list(
  nc_posX100_p3_k2,
  nc_posX100_p4_k3,
  nc_posX100_p5_k4
)
pos_kernels <- c("gamma", "lognormal", "invgauss", "amoroso")
backends <- c("crp", "sb")
mcmc <- default_mcmc()

for (obj in pos_sets) {
  print_dataset_header(obj)
  for (ker in pos_kernels) {
    for (backend in backends) {
      bundle <- build_nimble_bundle(
        y = obj$y,
        X = obj$X,
        backend = backend,
        kernel = ker,
        GPD = FALSE,
        components = obj$meta$K_true,
        mcmc = mcmc
      )
      summary(bundle)
      fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
      summary(fit)
    }
  }
}
#> head(y):
#> [1] 2.853109 1.332784 2.401409 3.538575 1.269099 4.359067
#> meta: n=100 | support=positive | p=3 | K_true=2 | tail=FALSE
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel         Gamma Distribution
#>  Components                          2
#>           N                        100
#>           X                  YES (P=3)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                  gamma     
#>           meta components info           model                      2     
#>           meta          N info           model                    100     
#>           meta          P info           model                      3     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk      shape dist component (1:2) gamma(shape=2, rate=1)     
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
#>   alpha, z[1:100], shape[1:2], scale[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#> CRP_cluster_wrapper sampler (4)
#>   - scale[]  (2 elements)
#>   - shape[]  (2 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>   [Warning] There are 2 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel     Gamma Distribution
#>  Components                      2
#>           N                    100
#>           X              YES (P=3)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk      shape dist component (1:2)
#>           bulk      scale link      regression
#>                              prior link                                notes
#>                                 sb                                          
#>                              gamma                                          
#>                                  2                                          
#>                                100                                          
#>                                  3                                          
#>             gamma(shape=1, rate=1)                  stochastic concentration
#>             gamma(shape=2, rate=1)                     iid across components
#>  beta_scale ~ normal(mean=0, sd=2)  exp beta_scale is 2 x 3 (components x P)
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:2], z[1:100], shape[1:2], beta_scale[1:2,1:3]
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
#> thin = 1: alpha, beta_scale, shape, w, z
#> ===== Samplers =====
#> RW sampler (10)
#>   - alpha
#>   - shape[]  (2 elements)
#>   - beta_scale[]  (6 elements)
#>   - v[]  (1 element)
#> categorical sampler (100)
#>   - z[]  (100 elements)
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
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel     Lognormal Distribution
#>  Components                          2
#>           N                        100
#>           X                  YES (P=3)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model              lognormal     
#>           meta components info           model                      2     
#>           meta          N info           model                    100     
#>           meta          P info           model                      3     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk    meanlog dist component (1:2)   normal(mean=0, sd=5)     
#>           bulk      sdlog dist component (1:2) gamma(shape=2, rate=1)     
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
#>   alpha, z[1:100], meanlog[1:2], sdlog[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#> CRP_cluster_wrapper sampler (4)
#>   - sdlog[]  (2 elements)
#>   - meanlog[]  (2 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>   [Warning] There are 1 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel Lognormal Distribution
#>  Components                      2
#>           N                    100
#>           X              YES (P=3)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk    meanlog link      regression
#>           bulk      sdlog dist component (1:2)
#>                                prior     link
#>                                   sb         
#>                            lognormal         
#>                                    2         
#>                                  100         
#>                                    3         
#>               gamma(shape=1, rate=1)         
#>  beta_meanlog ~ normal(mean=0, sd=2) identity
#>               gamma(shape=2, rate=1)         
#>                                   notes
#>                                        
#>                                        
#>                                        
#>                                        
#>                                        
#>                stochastic concentration
#>  beta_meanlog is 2 x 3 (components x P)
#>                   iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:2], z[1:100], beta_meanlog[1:2,1:3], sdlog[1:2]
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
#> thin = 1: alpha, beta_meanlog, sdlog, w, z
#> ===== Samplers =====
#> RW sampler (8)
#>   - alpha
#>   - beta_meanlog[]  (6 elements)
#>   - v[]  (1 element)
#> conjugate sampler (2)
#>   - sdlog[]  (2 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
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
#>   [Warning] There are 10 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                         Value
#>     Backend    Chinese Restaurant Process
#>      Kernel Inverse Gaussian Distribution
#>  Components                             2
#>           N                           100
#>           X                     YES (P=3)
#>         GPD                         FALSE
#>     Epsilon                         0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model               invgauss     
#>           meta components info           model                      2     
#>           meta          N info           model                    100     
#>           meta          P info           model                      3     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk       mean dist component (1:2) gamma(shape=2, rate=1)     
#>           bulk      shape dist component (1:2) gamma(shape=2, rate=1)     
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
#>   alpha, z[1:100], mean[1:2], shape[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#>   [Note] Registering 'dInvGauss' as a distribution based on its use in BUGS code. If you make changes to the nimbleFunctions for the distribution, you must call 'deregisterDistributions' before using the distribution in BUGS code for those changes to take effect.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
#> Checking model sizes and dimensions
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, mean, shape, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (4)
#>   - mean[]  (2 elements)
#>   - shape[]  (2 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>   [Warning] There are 2 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                         Value
#>     Backend        Stick-Breaking Process
#>      Kernel Inverse Gaussian Distribution
#>  Components                             2
#>           N                           100
#>           X                     YES (P=3)
#>         GPD                         FALSE
#>     Epsilon                         0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                            prior
#>           meta    backend info           model                               sb
#>           meta     kernel info           model                         invgauss
#>           meta components info           model                                2
#>           meta          N info           model                              100
#>           meta          P info           model                                3
#>  concentration      alpha dist          scalar           gamma(shape=1, rate=1)
#>           bulk       mean link      regression beta_mean ~ normal(mean=0, sd=2)
#>           bulk      shape dist component (1:2)           gamma(shape=2, rate=1)
#>  link                               notes
#>                                          
#>                                          
#>                                          
#>                                          
#>                                          
#>                  stochastic concentration
#>   exp beta_mean is 2 x 3 (components x P)
#>                     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:2], z[1:100], beta_mean[1:2,1:3], shape[1:2]
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
#> thin = 1: alpha, beta_mean, shape, w, z
#> ===== Samplers =====
#> RW sampler (10)
#>   - alpha
#>   - shape[]  (2 elements)
#>   - beta_mean[]  (6 elements)
#>   - v[]  (1 element)
#> categorical sampler (100)
#>   - z[]  (100 elements)
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
#>   [Warning] There are 18 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel       Amoroso Distribution
#>  Components                          2
#>           N                        100
#>           X                  YES (P=3)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                amoroso     
#>           meta components info           model                      2     
#>           meta          N info           model                    100     
#>           meta          P info           model                      3     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk        loc dist component (1:2)   normal(mean=0, sd=5)     
#>           bulk      scale dist component (1:2) gamma(shape=2, rate=1)     
#>           bulk     shape1 dist component (1:2) gamma(shape=2, rate=1)     
#>           bulk     shape2 dist component (1:2) gamma(shape=2, rate=1)     
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
#>   alpha, z[1:100], loc[1:2], scale[1:2], shape1[1:2], shape2[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#>   [Note] Registering 'dAmoroso' as a distribution based on its use in BUGS code. If you make changes to the nimbleFunctions for the distribution, you must call 'deregisterDistributions' before using the distribution in BUGS code for those changes to take effect.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
#> Checking model sizes and dimensions
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, loc, scale, shape1, shape2, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (8)
#>   - loc[]  (2 elements)
#>   - scale[]  (2 elements)
#>   - shape1[]  (2 elements)
#>   - shape2[]  (2 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>  Components                      2
#>           N                    100
#>           X              YES (P=3)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk        loc link      regression
#>           bulk      scale link      regression
#>           bulk     shape1 dist component (1:2)
#>           bulk     shape2 dist component (1:2)
#>                              prior     link
#>                                 sb         
#>                            amoroso         
#>                                  2         
#>                                100         
#>                                  3         
#>             gamma(shape=1, rate=1)         
#>    beta_loc ~ normal(mean=0, sd=2) identity
#>  beta_scale ~ normal(mean=0, sd=2)      exp
#>             gamma(shape=2, rate=1)         
#>             gamma(shape=2, rate=1)         
#>                                 notes
#>                                      
#>                                      
#>                                      
#>                                      
#>                                      
#>              stochastic concentration
#>    beta_loc is 2 x 3 (components x P)
#>  beta_scale is 2 x 3 (components x P)
#>                 iid across components
#>                 iid across components
#> 
#> Monitors
#>   n = 7 
#>   alpha, w[1:2], z[1:100], beta_loc[1:2,1:3], beta_scale[1:2,1:3], shape1[1:2], shape2[1:2]
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
#> thin = 1: alpha, beta_loc, beta_scale, shape1, shape2, w, z
#> ===== Samplers =====
#> RW sampler (18)
#>   - alpha
#>   - shape1[]  (2 elements)
#>   - shape2[]  (2 elements)
#>   - beta_loc[]  (6 elements)
#>   - beta_scale[]  (6 elements)
#>   - v[]  (1 element)
#> categorical sampler (100)
#>   - z[]  (100 elements)
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
#>   [Warning] There are 5 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> head(y):
#> [1] 1.8685352 1.9024758 1.6395495 4.6017516 2.8115051 0.6970118
#> meta: n=100 | support=positive | p=4 | K_true=3 | tail=FALSE
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel         Gamma Distribution
#>  Components                          3
#>           N                        100
#>           X                  YES (P=4)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                  gamma     
#>           meta components info           model                      3     
#>           meta          N info           model                    100     
#>           meta          P info           model                      4     
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
#>   alpha, z[1:100], shape[1:3], scale[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#>   - z[1:100]
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
#>   [Warning] There are 19 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel     Gamma Distribution
#>  Components                      3
#>           N                    100
#>           X              YES (P=4)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk      shape dist component (1:3)
#>           bulk      scale link      regression
#>                              prior link                                notes
#>                                 sb                                          
#>                              gamma                                          
#>                                  3                                          
#>                                100                                          
#>                                  4                                          
#>             gamma(shape=1, rate=1)                  stochastic concentration
#>             gamma(shape=2, rate=1)                     iid across components
#>  beta_scale ~ normal(mean=0, sd=2)  exp beta_scale is 3 x 4 (components x P)
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:3], z[1:100], shape[1:3], beta_scale[1:3,1:4]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_scale, shape, w, z
#> ===== Samplers =====
#> RW sampler (18)
#>   - alpha
#>   - shape[]  (3 elements)
#>   - beta_scale[]  (12 elements)
#>   - v[]  (2 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 6 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel     Lognormal Distribution
#>  Components                          3
#>           N                        100
#>           X                  YES (P=4)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model              lognormal     
#>           meta components info           model                      3     
#>           meta          N info           model                    100     
#>           meta          P info           model                      4     
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
#>   alpha, z[1:100], meanlog[1:3], sdlog[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#>   - z[1:100]
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
#>      Kernel Lognormal Distribution
#>  Components                      3
#>           N                    100
#>           X              YES (P=4)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk    meanlog link      regression
#>           bulk      sdlog dist component (1:3)
#>                                prior     link
#>                                   sb         
#>                            lognormal         
#>                                    3         
#>                                  100         
#>                                    4         
#>               gamma(shape=1, rate=1)         
#>  beta_meanlog ~ normal(mean=0, sd=2) identity
#>               gamma(shape=2, rate=1)         
#>                                   notes
#>                                        
#>                                        
#>                                        
#>                                        
#>                                        
#>                stochastic concentration
#>  beta_meanlog is 3 x 4 (components x P)
#>                   iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:3], z[1:100], beta_meanlog[1:3,1:4], sdlog[1:3]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_meanlog, sdlog, w, z
#> ===== Samplers =====
#> RW sampler (15)
#>   - alpha
#>   - beta_meanlog[]  (12 elements)
#>   - v[]  (2 elements)
#> conjugate sampler (3)
#>   - sdlog[]  (3 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 10 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                         Value
#>     Backend    Chinese Restaurant Process
#>      Kernel Inverse Gaussian Distribution
#>  Components                             3
#>           N                           100
#>           X                     YES (P=4)
#>         GPD                         FALSE
#>     Epsilon                         0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model               invgauss     
#>           meta components info           model                      3     
#>           meta          N info           model                    100     
#>           meta          P info           model                      4     
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
#>   alpha, z[1:100], mean[1:3], shape[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#>   - z[1:100]
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
#>   [Warning] There are 1 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                         Value
#>     Backend        Stick-Breaking Process
#>      Kernel Inverse Gaussian Distribution
#>  Components                             3
#>           N                           100
#>           X                     YES (P=4)
#>         GPD                         FALSE
#>     Epsilon                         0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                            prior
#>           meta    backend info           model                               sb
#>           meta     kernel info           model                         invgauss
#>           meta components info           model                                3
#>           meta          N info           model                              100
#>           meta          P info           model                                4
#>  concentration      alpha dist          scalar           gamma(shape=1, rate=1)
#>           bulk       mean link      regression beta_mean ~ normal(mean=0, sd=2)
#>           bulk      shape dist component (1:3)           gamma(shape=2, rate=1)
#>  link                               notes
#>                                          
#>                                          
#>                                          
#>                                          
#>                                          
#>                  stochastic concentration
#>   exp beta_mean is 3 x 4 (components x P)
#>                     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:3], z[1:100], beta_mean[1:3,1:4], shape[1:3]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_mean, shape, w, z
#> ===== Samplers =====
#> RW sampler (18)
#>   - alpha
#>   - shape[]  (3 elements)
#>   - beta_mean[]  (12 elements)
#>   - v[]  (2 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 5 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel       Amoroso Distribution
#>  Components                          3
#>           N                        100
#>           X                  YES (P=4)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                amoroso     
#>           meta components info           model                      3     
#>           meta          N info           model                    100     
#>           meta          P info           model                      4     
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
#>   alpha, z[1:100], loc[1:3], scale[1:3], shape1[1:3], shape2[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#>   - z[1:100]
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
#>   [Warning] There are 6 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel   Amoroso Distribution
#>  Components                      3
#>           N                    100
#>           X              YES (P=4)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk        loc link      regression
#>           bulk      scale link      regression
#>           bulk     shape1 dist component (1:3)
#>           bulk     shape2 dist component (1:3)
#>                              prior     link
#>                                 sb         
#>                            amoroso         
#>                                  3         
#>                                100         
#>                                  4         
#>             gamma(shape=1, rate=1)         
#>    beta_loc ~ normal(mean=0, sd=2) identity
#>  beta_scale ~ normal(mean=0, sd=2)      exp
#>             gamma(shape=2, rate=1)         
#>             gamma(shape=2, rate=1)         
#>                                 notes
#>                                      
#>                                      
#>                                      
#>                                      
#>                                      
#>              stochastic concentration
#>    beta_loc is 3 x 4 (components x P)
#>  beta_scale is 3 x 4 (components x P)
#>                 iid across components
#>                 iid across components
#> 
#> Monitors
#>   n = 7 
#>   alpha, w[1:3], z[1:100], beta_loc[1:3,1:4], beta_scale[1:3,1:4], shape1[1:3], shape2[1:3]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_loc, beta_scale, shape1, shape2, w, z
#> ===== Samplers =====
#> RW sampler (33)
#>   - alpha
#>   - shape1[]  (3 elements)
#>   - shape2[]  (3 elements)
#>   - beta_loc[]  (12 elements)
#>   - beta_scale[]  (12 elements)
#>   - v[]  (2 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 44 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> head(y):
#> [1] 0.8268801 3.3986338 1.9959363 0.7126420 0.9898186 1.2604720
#> meta: n=100 | support=positive | p=5 | K_true=4 | tail=FALSE
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel         Gamma Distribution
#>  Components                          4
#>           N                        100
#>           X                  YES (P=5)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                  gamma     
#>           meta components info           model                      4     
#>           meta          N info           model                    100     
#>           meta          P info           model                      5     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk      shape dist component (1:4) gamma(shape=2, rate=1)     
#>           bulk      scale dist component (1:4) gamma(shape=2, rate=1)     
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
#>   alpha, z[1:100], shape[1:4], scale[1:4]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#> CRP_cluster_wrapper sampler (8)
#>   - scale[]  (4 elements)
#>   - shape[]  (4 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>      Kernel     Gamma Distribution
#>  Components                      4
#>           N                    100
#>           X              YES (P=5)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk      shape dist component (1:4)
#>           bulk      scale link      regression
#>                              prior link                                notes
#>                                 sb                                          
#>                              gamma                                          
#>                                  4                                          
#>                                100                                          
#>                                  5                                          
#>             gamma(shape=1, rate=1)                  stochastic concentration
#>             gamma(shape=2, rate=1)                     iid across components
#>  beta_scale ~ normal(mean=0, sd=2)  exp beta_scale is 4 x 5 (components x P)
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:4], z[1:100], shape[1:4], beta_scale[1:4,1:5]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_scale, shape, w, z
#> ===== Samplers =====
#> RW sampler (28)
#>   - alpha
#>   - shape[]  (4 elements)
#>   - beta_scale[]  (20 elements)
#>   - v[]  (3 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel     Lognormal Distribution
#>  Components                          4
#>           N                        100
#>           X                  YES (P=5)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model              lognormal     
#>           meta components info           model                      4     
#>           meta          N info           model                    100     
#>           meta          P info           model                      5     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk    meanlog dist component (1:4)   normal(mean=0, sd=5)     
#>           bulk      sdlog dist component (1:4) gamma(shape=2, rate=1)     
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
#>   alpha, z[1:100], meanlog[1:4], sdlog[1:4]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#> CRP_cluster_wrapper sampler (8)
#>   - sdlog[]  (4 elements)
#>   - meanlog[]  (4 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>      Kernel Lognormal Distribution
#>  Components                      4
#>           N                    100
#>           X              YES (P=5)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk    meanlog link      regression
#>           bulk      sdlog dist component (1:4)
#>                                prior     link
#>                                   sb         
#>                            lognormal         
#>                                    4         
#>                                  100         
#>                                    5         
#>               gamma(shape=1, rate=1)         
#>  beta_meanlog ~ normal(mean=0, sd=2) identity
#>               gamma(shape=2, rate=1)         
#>                                   notes
#>                                        
#>                                        
#>                                        
#>                                        
#>                                        
#>                stochastic concentration
#>  beta_meanlog is 4 x 5 (components x P)
#>                   iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:4], z[1:100], beta_meanlog[1:4,1:5], sdlog[1:4]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_meanlog, sdlog, w, z
#> ===== Samplers =====
#> RW sampler (24)
#>   - alpha
#>   - beta_meanlog[]  (20 elements)
#>   - v[]  (3 elements)
#> conjugate sampler (4)
#>   - sdlog[]  (4 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 2 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                         Value
#>     Backend    Chinese Restaurant Process
#>      Kernel Inverse Gaussian Distribution
#>  Components                             4
#>           N                           100
#>           X                     YES (P=5)
#>         GPD                         FALSE
#>     Epsilon                         0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model               invgauss     
#>           meta components info           model                      4     
#>           meta          N info           model                    100     
#>           meta          P info           model                      5     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk       mean dist component (1:4) gamma(shape=2, rate=1)     
#>           bulk      shape dist component (1:4) gamma(shape=2, rate=1)     
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
#>   alpha, z[1:100], mean[1:4], shape[1:4]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
#> Checking model sizes and dimensions
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, mean, shape, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (8)
#>   - mean[]  (4 elements)
#>   - shape[]  (4 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>       Field                         Value
#>     Backend        Stick-Breaking Process
#>      Kernel Inverse Gaussian Distribution
#>  Components                             4
#>           N                           100
#>           X                     YES (P=5)
#>         GPD                         FALSE
#>     Epsilon                         0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                            prior
#>           meta    backend info           model                               sb
#>           meta     kernel info           model                         invgauss
#>           meta components info           model                                4
#>           meta          N info           model                              100
#>           meta          P info           model                                5
#>  concentration      alpha dist          scalar           gamma(shape=1, rate=1)
#>           bulk       mean link      regression beta_mean ~ normal(mean=0, sd=2)
#>           bulk      shape dist component (1:4)           gamma(shape=2, rate=1)
#>  link                               notes
#>                                          
#>                                          
#>                                          
#>                                          
#>                                          
#>                  stochastic concentration
#>   exp beta_mean is 4 x 5 (components x P)
#>                     iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:4], z[1:100], beta_mean[1:4,1:5], shape[1:4]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_mean, shape, w, z
#> ===== Samplers =====
#> RW sampler (28)
#>   - alpha
#>   - shape[]  (4 elements)
#>   - beta_mean[]  (20 elements)
#>   - v[]  (3 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 4 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel       Amoroso Distribution
#>  Components                          4
#>           N                        100
#>           X                  YES (P=5)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                amoroso     
#>           meta components info           model                      4     
#>           meta          N info           model                    100     
#>           meta          P info           model                      5     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk        loc dist component (1:4)   normal(mean=0, sd=5)     
#>           bulk      scale dist component (1:4) gamma(shape=2, rate=1)     
#>           bulk     shape1 dist component (1:4) gamma(shape=2, rate=1)     
#>           bulk     shape2 dist component (1:4) gamma(shape=2, rate=1)     
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
#>   alpha, z[1:100], loc[1:4], scale[1:4], shape1[1:4], shape2[1:4]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
#> Checking model sizes and dimensions
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, loc, scale, shape1, shape2, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (16)
#>   - loc[]  (4 elements)
#>   - scale[]  (4 elements)
#>   - shape1[]  (4 elements)
#>   - shape2[]  (4 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>  Components                      4
#>           N                    100
#>           X              YES (P=5)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk        loc link      regression
#>           bulk      scale link      regression
#>           bulk     shape1 dist component (1:4)
#>           bulk     shape2 dist component (1:4)
#>                              prior     link
#>                                 sb         
#>                            amoroso         
#>                                  4         
#>                                100         
#>                                  5         
#>             gamma(shape=1, rate=1)         
#>    beta_loc ~ normal(mean=0, sd=2) identity
#>  beta_scale ~ normal(mean=0, sd=2)      exp
#>             gamma(shape=2, rate=1)         
#>             gamma(shape=2, rate=1)         
#>                                 notes
#>                                      
#>                                      
#>                                      
#>                                      
#>                                      
#>              stochastic concentration
#>    beta_loc is 4 x 5 (components x P)
#>  beta_scale is 4 x 5 (components x P)
#>                 iid across components
#>                 iid across components
#> 
#> Monitors
#>   n = 7 
#>   alpha, w[1:4], z[1:100], beta_loc[1:4,1:5], beta_scale[1:4,1:5], shape1[1:4], shape2[1:4]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_loc, beta_scale, shape1, shape2, w, z
#> ===== Samplers =====
#> RW sampler (52)
#>   - alpha
#>   - shape1[]  (4 elements)
#>   - shape2[]  (4 elements)
#>   - beta_loc[]  (20 elements)
#>   - beta_scale[]  (20 elements)
#>   - v[]  (3 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 29 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
```


``` r
real_sets <- list(
  nc_realX100_p3_k2,
  nc_realX100_p5_k3
)
real_kernels <- c("normal", "laplace", "cauchy")
backends <- c("crp", "sb")
mcmc <- default_mcmc()

for (obj in real_sets) {
  print_dataset_header(obj)
  for (ker in real_kernels) {
    for (backend in backends) {
      bundle <- build_nimble_bundle(
        y = obj$y,
        X = obj$X,
        backend = backend,
        kernel = ker,
        GPD = FALSE,
        components = obj$meta$K_true,
        mcmc = mcmc
      )
      summary(bundle)
      fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
      summary(fit)
    }
  }
}
#> head(y):
#> [1] -3.04071813  0.50193781 -0.07377192  1.32921964 -1.35554282  0.85920865
#> meta: n=100 | support=real | p=3 | K_true=2 | tail=FALSE
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel        Normal Distribution
#>  Components                          2
#>           N                        100
#>           X                  YES (P=3)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                 normal     
#>           meta components info           model                      2     
#>           meta          N info           model                    100     
#>           meta          P info           model                      3     
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
#>   alpha, z[1:100], mean[1:2], sd[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#>   - z[1:100]
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
#>   [Warning] There are 30 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Normal Distribution
#>  Components                      2
#>           N                    100
#>           X              YES (P=3)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                            prior
#>           meta    backend info           model                               sb
#>           meta     kernel info           model                           normal
#>           meta components info           model                                2
#>           meta          N info           model                              100
#>           meta          P info           model                                3
#>  concentration      alpha dist          scalar           gamma(shape=1, rate=1)
#>           bulk       mean link      regression beta_mean ~ normal(mean=0, sd=2)
#>           bulk         sd dist component (1:2)           gamma(shape=2, rate=1)
#>      link                               notes
#>                                              
#>                                              
#>                                              
#>                                              
#>                                              
#>                      stochastic concentration
#>  identity beta_mean is 2 x 3 (components x P)
#>                         iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:2], z[1:100], beta_mean[1:2,1:3], sd[1:2]
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
#> thin = 1: alpha, beta_mean, sd, w, z
#> ===== Samplers =====
#> RW sampler (8)
#>   - alpha
#>   - beta_mean[]  (6 elements)
#>   - v[]  (1 element)
#> conjugate sampler (2)
#>   - sd[]  (2 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
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
#>   [Warning] There are 14 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel       Laplace Distribution
#>  Components                          2
#>           N                        100
#>           X                  YES (P=3)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                laplace     
#>           meta components info           model                      2     
#>           meta          N info           model                    100     
#>           meta          P info           model                      3     
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
#>   alpha, z[1:100], location[1:2], scale[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#>   - z[1:100]
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
#>   [Warning] There are 30 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel   Laplace Distribution
#>  Components                      2
#>           N                    100
#>           X              YES (P=3)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk   location link      regression
#>           bulk      scale dist component (1:2)
#>                                 prior     link
#>                                    sb         
#>                               laplace         
#>                                     2         
#>                                   100         
#>                                     3         
#>                gamma(shape=1, rate=1)         
#>  beta_location ~ normal(mean=0, sd=2) identity
#>                gamma(shape=2, rate=1)         
#>                                    notes
#>                                         
#>                                         
#>                                         
#>                                         
#>                                         
#>                 stochastic concentration
#>  beta_location is 2 x 3 (components x P)
#>                    iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:2], z[1:100], beta_location[1:2,1:3], scale[1:2]
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
#> thin = 1: alpha, beta_location, scale, w, z
#> ===== Samplers =====
#> RW sampler (8)
#>   - alpha
#>   - beta_location[]  (6 elements)
#>   - v[]  (1 element)
#> conjugate sampler (2)
#>   - scale[]  (2 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
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
#>   [Warning] There are 23 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel        Cauchy Distribution
#>  Components                          2
#>           N                        100
#>           X                  YES (P=3)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                 cauchy     
#>           meta components info           model                      2     
#>           meta          N info           model                    100     
#>           meta          P info           model                      3     
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
#>   alpha, z[1:100], location[1:2], scale[1:2]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#>   [Note] Registering 'dCauchy' as a distribution based on its use in BUGS code. If you make changes to the nimbleFunctions for the distribution, you must call 'deregisterDistributions' before using the distribution in BUGS code for those changes to take effect.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#>   - z[1:100]
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
#>   [Warning] There are 31 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Cauchy Distribution
#>  Components                      2
#>           N                    100
#>           X              YES (P=3)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk   location link      regression
#>           bulk      scale dist component (1:2)
#>                                 prior     link
#>                                    sb         
#>                                cauchy         
#>                                     2         
#>                                   100         
#>                                     3         
#>                gamma(shape=1, rate=1)         
#>  beta_location ~ normal(mean=0, sd=2) identity
#>                gamma(shape=2, rate=1)         
#>                                    notes
#>                                         
#>                                         
#>                                         
#>                                         
#>                                         
#>                 stochastic concentration
#>  beta_location is 2 x 3 (components x P)
#>                    iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:2], z[1:100], beta_location[1:2,1:3], scale[1:2]
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
#> thin = 1: alpha, beta_location, scale, w, z
#> ===== Samplers =====
#> RW sampler (10)
#>   - alpha
#>   - scale[]  (2 elements)
#>   - beta_location[]  (6 elements)
#>   - v[]  (1 element)
#> categorical sampler (100)
#>   - z[]  (100 elements)
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
#>   [Warning] There are 39 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> head(y):
#> [1] -0.4587679 -1.1343521  0.8578123 -1.4589745 -0.2797705  1.1146390
#> meta: n=100 | support=real | p=5 | K_true=3 | tail=FALSE
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel        Normal Distribution
#>  Components                          3
#>           N                        100
#>           X                  YES (P=5)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                 normal     
#>           meta components info           model                      3     
#>           meta          N info           model                    100     
#>           meta          P info           model                      5     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk       mean dist component (1:3)   normal(mean=0, sd=5)     
#>           bulk         sd dist component (1:3) gamma(shape=2, rate=1)     
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
#>   alpha, z[1:100], mean[1:3], sd[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#> CRP_cluster_wrapper sampler (6)
#>   - sd[]  (3 elements)
#>   - mean[]  (3 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>   [Warning] There are 13 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Normal Distribution
#>  Components                      3
#>           N                    100
#>           X              YES (P=5)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                            prior
#>           meta    backend info           model                               sb
#>           meta     kernel info           model                           normal
#>           meta components info           model                                3
#>           meta          N info           model                              100
#>           meta          P info           model                                5
#>  concentration      alpha dist          scalar           gamma(shape=1, rate=1)
#>           bulk       mean link      regression beta_mean ~ normal(mean=0, sd=2)
#>           bulk         sd dist component (1:3)           gamma(shape=2, rate=1)
#>      link                               notes
#>                                              
#>                                              
#>                                              
#>                                              
#>                                              
#>                      stochastic concentration
#>  identity beta_mean is 3 x 5 (components x P)
#>                         iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:3], z[1:100], beta_mean[1:3,1:5], sd[1:3]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_mean, sd, w, z
#> ===== Samplers =====
#> RW sampler (18)
#>   - alpha
#>   - beta_mean[]  (15 elements)
#>   - v[]  (2 elements)
#> conjugate sampler (3)
#>   - sd[]  (3 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 21 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel       Laplace Distribution
#>  Components                          3
#>           N                        100
#>           X                  YES (P=5)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                laplace     
#>           meta components info           model                      3     
#>           meta          N info           model                    100     
#>           meta          P info           model                      5     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk   location dist component (1:3)   normal(mean=0, sd=5)     
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
#>   alpha, z[1:100], location[1:3], scale[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
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
#> CRP_cluster_wrapper sampler (6)
#>   - scale[]  (3 elements)
#>   - location[]  (3 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>   [Warning] There are 34 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel   Laplace Distribution
#>  Components                      3
#>           N                    100
#>           X              YES (P=5)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk   location link      regression
#>           bulk      scale dist component (1:3)
#>                                 prior     link
#>                                    sb         
#>                               laplace         
#>                                     3         
#>                                   100         
#>                                     5         
#>                gamma(shape=1, rate=1)         
#>  beta_location ~ normal(mean=0, sd=2) identity
#>                gamma(shape=2, rate=1)         
#>                                    notes
#>                                         
#>                                         
#>                                         
#>                                         
#>                                         
#>                 stochastic concentration
#>  beta_location is 3 x 5 (components x P)
#>                    iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:3], z[1:100], beta_location[1:3,1:5], scale[1:3]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_location, scale, w, z
#> ===== Samplers =====
#> RW sampler (18)
#>   - alpha
#>   - beta_location[]  (15 elements)
#>   - v[]  (2 elements)
#> conjugate sampler (3)
#>   - scale[]  (3 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 28 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                      Value
#>     Backend Chinese Restaurant Process
#>      Kernel        Cauchy Distribution
#>  Components                          3
#>           N                        100
#>           X                  YES (P=5)
#>         GPD                      FALSE
#>     Epsilon                      0.025
#> 
#> Parameter specification
#>          block  parameter mode           level                  prior link
#>           meta    backend info           model                    crp     
#>           meta     kernel info           model                 cauchy     
#>           meta components info           model                      3     
#>           meta          N info           model                    100     
#>           meta          P info           model                      5     
#>  concentration      alpha dist          scalar gamma(shape=1, rate=1)     
#>           bulk   location dist component (1:3)   normal(mean=0, sd=5)     
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
#>   alpha, z[1:100], location[1:3], scale[1:3]
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> Setting data and initial values
#>   [Note] 'X' is provided in 'data' but is not a variable in the model and is being ignored.
#> Checking model sizes and dimensions
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, location, scale, z
#> ===== Samplers =====
#> CRP_concentration sampler (1)
#>   - alpha
#> CRP_cluster_wrapper sampler (6)
#>   - location[]  (3 elements)
#>   - scale[]  (3 elements)
#> CRP sampler (1)
#>   - z[1:100]
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
#>   [Warning] There are 44 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel    Cauchy Distribution
#>  Components                      3
#>           N                    100
#>           X              YES (P=5)
#>         GPD                  FALSE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode           level
#>           meta    backend info           model
#>           meta     kernel info           model
#>           meta components info           model
#>           meta          N info           model
#>           meta          P info           model
#>  concentration      alpha dist          scalar
#>           bulk   location link      regression
#>           bulk      scale dist component (1:3)
#>                                 prior     link
#>                                    sb         
#>                                cauchy         
#>                                     3         
#>                                   100         
#>                                     5         
#>                gamma(shape=1, rate=1)         
#>  beta_location ~ normal(mean=0, sd=2) identity
#>                gamma(shape=2, rate=1)         
#>                                    notes
#>                                         
#>                                         
#>                                         
#>                                         
#>                                         
#>                 stochastic concentration
#>  beta_location is 3 x 5 (components x P)
#>                    iid across components
#> 
#> Monitors
#>   n = 5 
#>   alpha, w[1:3], z[1:100], beta_location[1:3,1:5], scale[1:3]
#> Defining model
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, beta_location, scale, w, z
#> ===== Samplers =====
#> RW sampler (21)
#>   - alpha
#>   - scale[]  (3 elements)
#>   - beta_location[]  (15 elements)
#>   - v[]  (2 elements)
#> categorical sampler (100)
#>   - z[]  (100 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#> running chain 2...
#>   [Warning] There are 25 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
```
