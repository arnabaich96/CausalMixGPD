---
title: "Developer guide"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Developer guide}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

Notes for extending the package: registering new kernels, testing predictive helpers, and inspecting example fits.




``` r
library(DPmixGPD)
library(nimble)
use_cached_fit <- TRUE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_small <- readRDS(.fit_path("fit_small.rds"))
library(ggplot2)
```

## Kernel template (copy/paste)


``` r
# # Dummy skeleton (copy into R/0-base-kernels.R and register)
# nimbleFunction(
#   run = function(y = double(1), mu = double(0), sigma = double(0)) {
#     returnType(double(0))
#     dnorm(y, mu, sigma, log = 1)
#   }
# )
```

## Registry entry template


``` r
# init_kernel_registry("custom", list(
#   density = "dCustomMix",
#   quantile = "qCustomMix",
#   rng = "rCustomMix",
#   support = "(0, Inf)",
#   params = c("mu", "sigma")
# ))
```

## Predictive sanity check


``` r
y <- sim_bulk_tail(n = 80, seed = 123)
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "lognormal",
  GPD = FALSE,
  J = 4,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 2, seed = c(1, 2))
)
if (use_cached_fit) {
  fit <- fit_small
} else {
  fit <- run_mcmc_bundle_manual(bundle)
}
predict(fit, type = "quantile", p = c(0.5, 0.9))
#> $fit
#>          [,1]     [,2]
#> [1,] 2.444721 9.102755
#> 
#> $lower
#> NULL
#> 
#> $upper
#> NULL
#> 
#> $type
#> [1] "quantile"
#> 
#> $grid
#> [1] 0.5 0.9
#> 
#> $draws
#> , , 1
#> 
#>           [,1]
#>  [1,] 2.084684
#>  [2,] 1.761678
#>  [3,] 1.711988
#>  [4,] 2.488099
#>  [5,] 2.076856
#>  [6,] 1.980510
#>  [7,] 1.814564
#>  [8,] 2.015153
#>  [9,] 2.182712
#> [10,] 2.031181
#> [11,] 2.258826
#> [12,] 2.569372
#> [13,] 2.218166
#> [14,] 2.193250
#> [15,] 2.682655
#> [16,] 2.117570
#> [17,] 2.296713
#> [18,] 2.129176
#> [19,] 2.790298
#> [20,] 2.822541
#> [21,] 3.457669
#> [22,] 2.982656
#> [23,] 2.594728
#> [24,] 2.355118
#> [25,] 2.245933
#> [26,] 2.183629
#> [27,] 1.964829
#> [28,] 2.134337
#> [29,] 2.835128
#> [30,] 2.387545
#> [31,] 2.352759
#> [32,] 3.252944
#> [33,] 3.306005
#> [34,] 2.716700
#> [35,] 2.742817
#> [36,] 2.507197
#> [37,] 2.553163
#> [38,] 3.063924
#> [39,] 3.017153
#> [40,] 2.908632
#> 
#> , , 2
#> 
#>            [,1]
#>  [1,]  6.922057
#>  [2,]  9.929835
#>  [3,]  7.849458
#>  [4,]  7.269094
#>  [5,]  8.674310
#>  [6,]  7.781911
#>  [7,]  8.536297
#>  [8,] 10.888322
#>  [9,] 11.407774
#> [10,] 10.809059
#> [11,]  9.291378
#> [12,]  9.973406
#> [13,] 10.977226
#> [14,]  8.564296
#> [15,] 10.137487
#> [16,]  9.616081
#> [17,] 10.518063
#> [18,]  9.349740
#> [19,]  7.093746
#> [20,]  7.202073
#> [21,]  7.656602
#> [22,]  7.060898
#> [23,]  8.188712
#> [24,] 10.574160
#> [25,]  9.003591
#> [26,] 11.601075
#> [27,] 12.098963
#> [28,] 11.249864
#> [29,]  9.996349
#> [30,] 11.637079
#> [31,]  9.994232
#> [32,]  8.581701
#> [33,]  8.387645
#> [34,]  8.502619
#> [35,]  9.427534
#> [36,]  8.093170
#> [37,]  6.892583
#> [38,]  7.841883
#> [39,]  6.846457
#> [40,]  7.683458
```

## Quick visualization


``` r
data.frame(y = y) |>
  ggplot(aes(x = y)) +
  geom_freqpoly(color = "steelblue") +
  labs(title = "Developer-mode data distribution", x = "y", y = "Count")
```

![](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\vignettes\v10-developer-guide_files/figure-html/developer-plot-1.png)<!-- -->

## Session info


``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31 ucrt)
#> Platform: x86_64-w64-mingw32/x64
#> Running under: Windows 11 x64 (build 26100)
#> 
#> Matrix products: default
#>   LAPACK version 3.12.1
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.utf8 
#> [2] LC_CTYPE=English_United States.utf8   
#> [3] LC_MONETARY=English_United States.utf8
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.utf8    
#> 
#> time zone: America/New_York
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices datasets  utils     methods   base     
#> 
#> other attached packages:
#> [1] devtools_2.4.6 usethis_3.2.1  dplyr_1.1.4    ggplot2_4.0.1  nimble_1.4.0  
#> [6] DPmixGPD_0.0.8 testthat_3.3.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6        xfun_0.55           bslib_0.9.0        
#>  [4] remotes_2.5.0       GGally_2.4.0        lattice_0.22-7     
#>  [7] numDeriv_2016.8-1.1 vctrs_0.6.5         tools_4.5.2        
#> [10] generics_0.1.4      parallel_4.5.2      tibble_3.3.0       
#> [13] pkgconfig_2.0.3     RColorBrewer_1.1-3  ggmcmc_1.5.1.2     
#> [16] S7_0.2.1            desc_1.4.3          lifecycle_1.0.4    
#> [19] compiler_4.5.2      farver_2.1.2        brio_1.1.5         
#> [22] codetools_0.2-20    htmltools_0.5.9     sass_0.4.10        
#> [25] yaml_2.3.12         pracma_2.4.6        jquerylib_0.1.4    
#> [28] pillar_1.11.1       tidyr_1.3.2         MASS_7.3-65        
#> [31] ellipsis_0.3.2      cachem_1.1.0        sessioninfo_1.2.3  
#> [34] parallelly_1.46.0   ggstats_0.11.0      tidyselect_1.2.1   
#> [37] digest_0.6.39       future_1.68.0       purrr_1.2.0        
#> [40] listenv_0.10.0      labeling_0.4.3      rprojroot_2.1.1    
#> [43] fastmap_1.2.0       grid_4.5.2          cli_3.6.5          
#> [46] magrittr_2.0.4      pkgbuild_1.4.8      future.apply_1.20.1
#> [49] withr_3.0.2         scales_1.4.0        rmarkdown_2.30     
#> [52] globals_0.18.0      igraph_2.2.1        otel_0.2.0         
#> [55] coda_0.19-4.1       memoise_2.0.1       evaluate_1.0.5     
#> [58] knitr_1.51          viridisLite_0.4.2   rlang_1.1.6        
#> [61] glue_1.8.0          renv_1.1.5          pkgload_1.4.1      
#> [64] jsonlite_2.0.0      rstudioapi_0.17.1   R6_2.6.1           
#> [67] fs_1.6.6
```
