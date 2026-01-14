# Kernels guide

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
library(dplyr)
```

``` r
y <- sim_bulk_tail(n = 160, seed = 77)
```

``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "amoroso",
  GPD = TRUE,
  J = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 2, seed = c(1, 2))
)
```

``` r
if (use_cached_fit) {
  fit <- fit_small
} else {
  fit <- run_mcmc_bundle_manual(bundle)
}
```

``` r
data.frame(y = y) |>
  ggplot(aes(x = y)) +
  geom_histogram(boundary = 0, bins = 30, fill = "coral", alpha = 0.6) +
  labs(title = "Data histogram showing bulk + tail", x = "y", y = "Count")
```

![](v04-kernels-guide_files/figure-html/kernel-viz-data-1.png)

``` r
predicted <- predict(fit, type = "quantile", p = c(0.5, 0.9, 0.99))
predicted
#> $fit
#>          [,1]     [,2]    [,3]
#> [1,] 2.444721 9.102755 10.1547
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
#> [1] 0.50 0.90 0.99
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
#> [1] dplyr_1.1.4    ggplot2_4.0.1  nimble_1.4.0   DPmixGPD_0.0.8
#> 
#> loaded via a namespace (and not attached):
#>  [1] sass_0.4.10         future_1.68.0       generics_0.1.4     
#>  [4] renv_1.1.5          lattice_0.22-7      listenv_0.10.0     
#>  [7] pracma_2.4.6        digest_0.6.39       magrittr_2.0.3     
#> [10] evaluate_1.0.5      grid_4.5.2          RColorBrewer_1.1-3 
#> [13] fastmap_1.2.0       jsonlite_2.0.0      scales_1.4.0       
#> [16] codetools_0.2-20    numDeriv_2016.8-1.1 textshaping_1.0.1  
#> [19] jquerylib_0.1.4     cli_3.6.5           rlang_1.1.6        
#> [22] parallelly_1.46.1   future.apply_1.20.1 withr_3.0.2        
#> [25] cachem_1.1.0        yaml_2.3.12         tools_4.5.2        
#> [28] parallel_4.5.2      coda_0.19-4.1       globals_0.18.0     
#> [31] vctrs_0.6.5         R6_2.6.1            lifecycle_1.0.5    
#> [34] fs_1.6.6            htmlwidgets_1.6.4   ragg_1.5.0         
#> [37] pkgconfig_2.0.3     desc_1.4.3          pkgdown_2.1.3      
#> [40] bslib_0.9.0         pillar_1.11.1       gtable_0.3.6       
#> [43] glue_1.8.0          systemfonts_1.2.3   xfun_0.52          
#> [46] tibble_3.3.0        tidyselect_1.2.1    knitr_1.50         
#> [49] farver_2.1.2        htmltools_0.5.8.1   igraph_2.2.1       
#> [52] labeling_0.4.3      rmarkdown_2.30      compiler_4.5.2     
#> [55] S7_0.2.1
```
