---
title: "GPD Tail"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{GPD Tail}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
data("nc_pos_tail200_k4")
print_dataset_header(nc_pos_tail200_k4, "nc_pos_tail200_k4")
#> 
#> Dataset: nc_pos_tail200_k4 
#> head(y):
#> [1] 0.7440164 1.2666728 2.2842959 0.6045776 0.9406917 1.5790861
#> meta: n=200 | support=positive | p=0 | K_true=4 | tail=TRUE
```


``` r
# Keep the example runnable: one kernel/backend with a tiny MCMC.
mcmc <- list(niter = 80, nburnin = 20, thin = 1, nchains = 1, seed = 1)

bundle <- build_nimble_bundle(
  y = nc_pos_tail200_k4$y,
  backend = "sb",
  kernel = "gamma",
  GPD = TRUE,
  components = nc_pos_tail200_k4$meta$K_true,
  mcmc = mcmc
)
summary(bundle)
#> DPmixGPD bundle summary
#>       Field                  Value
#>     Backend Stick-Breaking Process
#>      Kernel     Gamma Distribution
#>  Components                      4
#>           N                    200
#>           X                     NO
#>         GPD                   TRUE
#>     Epsilon                  0.025
#> 
#> Parameter specification
#>          block  parameter mode             level                  prior link
#>           meta    backend info             model                     sb     
#>           meta     kernel info             model                  gamma     
#>           meta components info             model                      4     
#>           meta          N info             model                    200     
#>           meta          P info             model                      0     
#>  concentration      alpha dist            scalar gamma(shape=1, rate=1)     
#>           bulk      shape dist   component (1:4) gamma(shape=2, rate=1)     
#>           bulk      scale dist   component (1:4) gamma(shape=2, rate=1)     
#>            gpd  threshold dist observation (1:N) gamma(shape=2, rate=1)     
#>            gpd tail_scale dist            scalar gamma(shape=2, rate=1)     
#>            gpd tail_shape dist            scalar normal(mean=0, sd=0.2)     
#>                      notes
#>                           
#>                           
#>                           
#>                           
#>                           
#>   stochastic concentration
#>      iid across components
#>      iid across components
#>  threshold[i] iid across i
#>                           
#>                           
#> 
#> Monitors
#>   n = 8 
#>   alpha, w[1:4], z[1:200], shape[1:4], scale[1:4], threshold[1:200], tail_scale, tail_shape

fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#>   [Note] Registering 'dGammaGpd' as a distribution based on its use in BUGS code. If you make changes to the nimbleFunctions for the distribution, you must call 'deregisterDistributions' before using the distribution in BUGS code for those changes to take effect.
#> Building model
#> Setting data and initial values
#> Checking model sizes and dimensions
#>   [Note] This model is not fully initialized. This is not an error.
#>          To see which variables are not initialized, use model$initializeInfo().
#>          For more information on model initialization, see help(modelInitialization).
#> Checking model calculations
#> ===== Monitors =====
#> thin = 1: alpha, scale, shape, tail_scale, tail_shape, threshold, w, z
#> ===== Samplers =====
#> RW sampler (214)
#>   - alpha
#>   - shape[]  (4 elements)
#>   - scale[]  (4 elements)
#>   - threshold[]  (200 elements)
#>   - tail_scale
#>   - tail_shape
#>   - v[]  (3 elements)
#> categorical sampler (200)
#>   - z[]  (200 elements)
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> Compiling
#>   [Note] This may take a minute.
#>   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.
#> running chain 1...
#>   [Warning] There are 25 individual pWAIC values that are greater than 0.4. This may indicate that the WAIC estimate is unstable (Vehtari et al., 2017), at least in cases without grouping of data nodes or multivariate data nodes.
summary(fit)
#> MixGPD summary | backend: Stick-Breaking Process | kernel: Gamma Distribution | GPD tail: TRUE | epsilon: 0.025
#> n = 200 | components = 4
#> Summary
#> Initial components: 4 | Components after truncation: 4
#> 
#> WAIC: 668.452
#> lppd: -289.658 | pWAIC: 44.568
#> 
#> Summary table
#> Showing first 60 of 215 parameters.
#> 
#>      parameter  mean    sd q0.025 q0.500 q0.975    ess
#>     weights[1] 0.699 0.037  0.627  0.702  0.758  7.192
#>     weights[2] 0.161 0.027  0.117  0.160  0.215 17.866
#>     weights[3] 0.104 0.027  0.052  0.100  0.153 24.118
#>     weights[4] 0.050 0.018  0.030  0.045  0.089 15.953
#>          alpha 1.064 0.427  0.211  1.038  1.909 21.567
#>     tail_scale 2.070 0.352  1.454  2.218  2.593  4.322
#>     tail_shape 0.000 0.000  0.000  0.000  0.000  0.000
#>   threshold[1] 1.811 0.960  0.362  1.611  3.828  8.417
#>   threshold[2] 2.442 1.292  0.500  2.358  4.962  7.804
#>   threshold[3] 0.936 0.662  0.154  0.737  2.423 20.207
#>   threshold[4] 1.092 0.776  0.135  0.807  2.898  9.955
#>   threshold[5] 1.470 0.787  0.251  1.434  2.911  7.467
#>   threshold[6] 2.283 1.153  0.553  1.970  4.155  6.230
#>   threshold[7] 1.821 1.121  0.332  1.769  4.305  4.420
#>   threshold[8] 1.380 0.983  0.303  1.072  3.657  3.170
#>   threshold[9] 2.571 1.539  0.400  2.110  6.110  6.383
#>  threshold[10] 1.624 0.855  0.387  1.447  3.496  7.432
#>  threshold[11] 1.605 0.993  0.270  1.501  3.930  6.479
#>  threshold[12] 2.245 1.327  0.615  2.033  4.615  4.697
#>  threshold[13] 1.403 1.197  0.125  1.114  3.647  4.079
#>  threshold[14] 1.568 0.877  0.364  1.311  3.406 10.783
#>  threshold[15] 2.096 1.529  0.060  1.363  5.368  3.992
#>  threshold[16] 3.019 2.232  0.250  2.762  7.399  2.296
#>  threshold[17] 2.408 1.578  0.555  1.961  6.049  3.112
#>  threshold[18] 1.537 1.132  0.180  1.228  4.548  9.214
#>  threshold[19] 2.016 1.077  0.656  1.823  4.967 15.856
#>  threshold[20] 3.119 1.660  0.166  3.438  5.464  3.875
#>  threshold[21] 1.738 0.962  0.308  1.809  4.449 11.552
#>  threshold[22] 2.569 2.215  0.241  2.071  6.472  2.226
#>  threshold[23] 2.367 1.283  0.777  2.227  4.378  5.053
#>  threshold[24] 2.366 1.228  0.815  2.168  5.060  5.314
#>  threshold[25] 2.481 1.123  0.753  2.479  4.365  8.635
#>  threshold[26] 2.274 0.924  0.855  2.288  4.161 15.078
#>  threshold[27] 2.236 0.858  0.756  2.187  3.849 12.031
#>  threshold[28] 2.438 1.547  0.315  1.749  5.461  5.534
#>  threshold[29] 1.241 0.748  0.313  1.110  2.648 20.420
#>  threshold[30] 2.103 1.673  0.305  1.546  5.710  2.268
#>  threshold[31] 1.877 0.946  0.515  1.752  3.667 12.738
#>  threshold[32] 2.139 1.568  0.250  1.598  5.295  3.060
#>  threshold[33] 2.366 1.054  0.628  2.446  4.685 10.099
#>  threshold[34] 1.769 0.885  0.550  1.566  3.831 10.175
#>  threshold[35] 1.872 1.578  0.269  1.350  5.740  2.385
#>  threshold[36] 1.460 0.931  0.286  1.273  3.582 35.292
#>  threshold[37] 3.103 1.318  0.895  2.837  5.457  4.533
#>  threshold[38] 2.104 1.022  0.539  1.945  3.943  8.892
#>  threshold[39] 1.929 1.373  0.405  1.517  5.305  5.945
#>  threshold[40] 1.472 0.854  0.632  1.333  3.556  9.972
#>  threshold[41] 1.557 1.444  0.172  1.225  5.708  3.617
#>  threshold[42] 3.352 1.851  0.763  3.409  6.381  2.993
#>  threshold[43] 1.727 0.915  0.237  1.631  3.671 10.548
#>  threshold[44] 1.230 0.839  0.286  0.960  3.235 10.884
#>  threshold[45] 2.450 1.172  0.071  2.258  4.332  6.314
#>  threshold[46] 2.913 1.562  0.757  2.517  6.270  4.169
#>  threshold[47] 1.669 1.229  0.346  1.363  4.936  7.399
#>  threshold[48] 1.423 0.999  0.077  1.296  3.798  7.040
#>  threshold[49] 1.661 0.933  0.347  1.566  3.559 16.887
#>  threshold[50] 1.597 0.934  0.406  1.722  3.038 12.520
#>  threshold[51] 1.389 0.677  0.033  1.566  2.330  9.872
#>  threshold[52] 1.287 0.772  0.310  1.121  3.007  9.660
#>  threshold[53] 2.470 1.673  0.218  2.746  5.532  5.144

# Light prediction on the observed grid
pred <- predict(fit, y = sort(nc_pos_tail200_k4$y), type = "survival")
head(pred$fit)
#>           [,1]      [,2]      [,3]     [,4]      [,5]      [,6]      [,7]
#> [1,] 0.9751857 0.9646329 0.9558366 0.949212 0.9461769 0.9445516 0.9311559
#>          [,8]     [,9]     [,10]    [,11]     [,12]     [,13]     [,14]
#> [1,] 0.927469 0.926026 0.9256114 0.920515 0.9194294 0.9047972 0.9022912
#>         [,15]     [,16]     [,17]     [,18]     [,19]     [,20]     [,21]
#> [1,] 0.896955 0.8894515 0.8890068 0.8833633 0.8719988 0.8640291 0.8639183
#>          [,22]     [,23]     [,24]     [,25]     [,26]     [,27]     [,28]
#> [1,] 0.8565307 0.8548975 0.8524257 0.8509428 0.8508706 0.8476763 0.8415628
#>          [,29]     [,30]     [,31]     [,32]     [,33]     [,34]     [,35]
#> [1,] 0.8324945 0.8315025 0.8286593 0.8249663 0.8210806 0.8198834 0.8131198
#>          [,36]     [,37]     [,38]     [,39]     [,40]     [,41]     [,42]
#> [1,] 0.8099058 0.8096479 0.8075282 0.7956037 0.7943602 0.7941248 0.7929159
#>          [,43]    [,44]    [,45]     [,46]     [,47]     [,48]     [,49]
#> [1,] 0.7898863 0.788292 0.783104 0.7819977 0.7809845 0.7725435 0.7711841
#>          [,50]     [,51]     [,52]     [,53]     [,54]     [,55]     [,56]
#> [1,] 0.7690921 0.7662141 0.7596583 0.7579269 0.7575779 0.7571366 0.7506561
#>          [,57]     [,58]     [,59]     [,60]     [,61]     [,62]    [,63]
#> [1,] 0.7493296 0.7461626 0.7431416 0.7410793 0.7349771 0.7330979 0.730053
#>          [,64]     [,65]     [,66]     [,67]     [,68]    [,69]    [,70]
#> [1,] 0.7259461 0.7161735 0.7153931 0.7126313 0.7087905 0.704115 0.694209
#>          [,71]     [,72]     [,73]    [,74]     [,75]     [,76]     [,77]
#> [1,] 0.6937132 0.6931544 0.6918717 0.684619 0.6801804 0.6801559 0.6793994
#>          [,78]     [,79]     [,80]     [,81]     [,82]     [,83]     [,84]
#> [1,] 0.6620671 0.6551932 0.6526201 0.6524336 0.6489874 0.6462605 0.6377126
#>          [,85]     [,86]     [,87]    [,88]     [,89]     [,90]     [,91]
#> [1,] 0.6358591 0.6285575 0.6245335 0.612726 0.6113681 0.6113422 0.6051633
#>          [,92]     [,93]     [,94]     [,95]     [,96]     [,97]    [,98]
#> [1,] 0.5997983 0.5987003 0.5918598 0.5852695 0.5836529 0.5823284 0.576666
#>          [,99]    [,100]    [,101]    [,102]    [,103]    [,104]    [,105]
#> [1,] 0.5749106 0.5621325 0.5578873 0.5547591 0.5518663 0.5479954 0.5453346
#>         [,106]    [,107]    [,108]    [,109]    [,110]    [,111]   [,112]
#> [1,] 0.5419031 0.5409377 0.5398261 0.5340236 0.5186435 0.5098575 0.498943
#>         [,113]    [,114]    [,115]    [,116]    [,117]    [,118]    [,119]
#> [1,] 0.4982783 0.4966028 0.4821655 0.4631726 0.4566852 0.4417339 0.4368915
#>         [,120]    [,121]    [,122]   [,123]    [,124]    [,125]    [,126]
#> [1,] 0.4305832 0.4272952 0.4254333 0.421904 0.4200697 0.4149962 0.4148333
#>         [,127]    [,128]   [,129]    [,130]    [,131]    [,132]    [,133]
#> [1,] 0.4078314 0.4014971 0.394245 0.3922998 0.3856596 0.3778242 0.3773286
#>         [,134]    [,135]    [,136]    [,137]    [,138]    [,139]    [,140]
#> [1,] 0.3705942 0.3621689 0.3453786 0.3336777 0.3320185 0.3051495 0.3037037
#>         [,141]    [,142]    [,143]    [,144]    [,145]    [,146]    [,147]
#> [1,] 0.2967258 0.2866695 0.2822619 0.2760226 0.2628062 0.2581645 0.2575465
#>         [,148]    [,149]    [,150]    [,151]    [,152]    [,153]    [,154]
#> [1,] 0.2559139 0.2545977 0.2500473 0.2440936 0.2384375 0.2238155 0.2227282
#>         [,155]    [,156]    [,157]    [,158]    [,159]    [,160]    [,161]
#> [1,] 0.2148825 0.2125503 0.2025375 0.2025241 0.1994061 0.1983971 0.1967576
#>         [,162]    [,163]    [,164]    [,165]    [,166]    [,167]    [,168]
#> [1,] 0.1891965 0.1836554 0.1811207 0.1809237 0.1712093 0.1711664 0.1687112
#>         [,169]    [,170]    [,171]    [,172]    [,173]    [,174]    [,175]
#> [1,] 0.1628478 0.1607293 0.1601357 0.1499517 0.1477841 0.1419221 0.1416136
#>         [,176]    [,177]   [,178]   [,179]    [,180]    [,181]    [,182]
#> [1,] 0.1352323 0.1242836 0.123959 0.122708 0.1175534 0.1113352 0.1102777
#>         [,183]    [,184]     [,185]   [,186]   [,187]     [,188]     [,189]
#> [1,] 0.1060329 0.1023999 0.09882872 0.096855 0.088929 0.08629677 0.08502314
#>          [,190]     [,191]     [,192]     [,193]     [,194]     [,195]
#> [1,] 0.08410911 0.06262982 0.04808672 0.02112197 0.01512756 0.01472665
#>         [,196]     [,197]      [,198]      [,199]       [,200]
#> [1,] 0.0146706 0.01121765 0.008541787 0.005006357 0.0001254525
```
