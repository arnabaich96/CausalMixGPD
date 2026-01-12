---
title: "Kernel: Gamma"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Kernel: Gamma}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




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
w <- c(0.4, 0.6)
shape <- c(2.0, 4.0)
scale <- c(0.5, 0.8)

x <- 1.0
p <- 0.9

dGammaMix(x, w = w, shape = shape, scale = scale, log = FALSE)
#> [1] 0.2864839
pGammaMix(x, w = w, shape = shape, scale = scale, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.2605591
qGammaMix(p, w = w, shape = shape, scale = scale)
#> [1] 4.671995

threshold <- 1.1
tail_scale <- 0.9
tail_shape <- 0.2

dGammaMixGpd(x, w = w, shape = shape, scale = scale,
             threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 0.2864839
qGammaMixGpd(0.99, w = w, shape = shape, scale = scale,
             threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 7.158889

set.seed(1)
y <- rgamma(30, shape = 2, scale = 0.8)
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "gamma",
  GPD = TRUE,
  J = 6
)

bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "gamma"
#> 
#> $GPD
#> [1] TRUE
#> 
#> $has_X
#> [1] FALSE
#> 
#> $N
#> [1] 30
#> 
#> $P
#> [1] 0
#> 
#> $components
#> [1] 6
#> 
#> $custom_build
#> [1] FALSE
```


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
#> [1] nimble_1.4.0   DPmixGPD_0.0.8 testthat_3.3.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.6        xfun_0.55           bslib_0.9.0        
#>  [4] ggplot2_4.0.1       devtools_2.4.6      remotes_2.5.0      
#>  [7] GGally_2.4.0        lattice_0.22-7      numDeriv_2016.8-1.1
#> [10] vctrs_0.6.5         tools_4.5.2         generics_0.1.4     
#> [13] parallel_4.5.2      tibble_3.3.0        pkgconfig_2.0.3    
#> [16] RColorBrewer_1.1-3  ggmcmc_1.5.1.2      S7_0.2.1           
#> [19] desc_1.4.3          lifecycle_1.0.4     compiler_4.5.2     
#> [22] farver_2.1.2        brio_1.1.5          codetools_0.2-20   
#> [25] htmltools_0.5.9     usethis_3.2.1       sass_0.4.10        
#> [28] yaml_2.3.12         pracma_2.4.6        jquerylib_0.1.4    
#> [31] pillar_1.11.1       tidyr_1.3.2         MASS_7.3-65        
#> [34] ellipsis_0.3.2      cachem_1.1.0        sessioninfo_1.2.3  
#> [37] parallelly_1.46.0   ggstats_0.11.0      tidyselect_1.2.1   
#> [40] digest_0.6.39       future_1.68.0       dplyr_1.1.4        
#> [43] purrr_1.2.0         listenv_0.10.0      labeling_0.4.3     
#> [46] rprojroot_2.1.1     fastmap_1.2.0       grid_4.5.2         
#> [49] cli_3.6.5           magrittr_2.0.4      pkgbuild_1.4.8     
#> [52] future.apply_1.20.1 withr_3.0.2         scales_1.4.0       
#> [55] rmarkdown_2.30      globals_0.18.0      igraph_2.2.1       
#> [58] otel_0.2.0          coda_0.19-4.1       memoise_2.0.1      
#> [61] evaluate_1.0.5      knitr_1.51          rlang_1.1.6        
#> [64] glue_1.8.0          renv_1.1.5          pkgload_1.4.1      
#> [67] jsonlite_2.0.0      rstudioapi_0.17.1   R6_2.6.1           
#> [70] fs_1.6.6
```
