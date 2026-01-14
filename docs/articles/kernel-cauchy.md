# Kernel: Cauchy

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
```

``` r
w <- c(0.5, 0.5)
location <- c(-0.5, 0.5)
scale <- c(0.7, 1.0)

x <- 0.2
p <- 0.9
```

``` r
dCauchyMix(x, w = w, location = location, scale = scale, log = FALSE)
#> [1] 0.2596958
pCauchyMix(x, w = w, location = location, scale = scale, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.5786132
qCauchyMix(p, w = w, location = location, scale = scale)
#> [1] 2.759006
```

``` r
set.seed(1)
y <- abs(rcauchy(30)) + 0.1
```

``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "cauchy",
  GPD = FALSE,
  J = 6
)
```

``` r
bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "cauchy"
#> 
#> $GPD
#> [1] FALSE
#> 
#> $has_X
#> [1] FALSE
#> 
#> $has_ps
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
#> [1] testthat_3.3.1 nimble_1.4.0   DPmixGPD_0.0.8
#> 
#> loaded via a namespace (and not attached):
#>  [1] future.apply_1.20.1 jsonlite_2.0.0      compiler_4.5.2     
#>  [4] renv_1.1.5          brio_1.1.5          parallel_4.5.2     
#>  [7] jquerylib_0.1.4     globals_0.18.0      systemfonts_1.3.1  
#> [10] textshaping_1.0.4   yaml_2.3.12         fastmap_1.2.0      
#> [13] lattice_0.22-7      coda_0.19-4.1       R6_2.6.1           
#> [16] igraph_2.2.1        knitr_1.51          htmlwidgets_1.6.4  
#> [19] future_1.68.0       desc_1.4.3          bslib_0.9.0        
#> [22] rlang_1.1.6         cachem_1.1.0        xfun_0.55          
#> [25] fs_1.6.6            sass_0.4.10         otel_0.2.0         
#> [28] cli_3.6.5           pkgdown_2.2.0       magrittr_2.0.4     
#> [31] digest_0.6.39       grid_4.5.2          lifecycle_1.0.4    
#> [34] evaluate_1.0.5      pracma_2.4.6        numDeriv_2016.8-1.1
#> [37] listenv_0.10.0      codetools_0.2-20    ragg_1.5.0         
#> [40] parallelly_1.46.0   rmarkdown_2.30      tools_4.5.2        
#> [43] pkgconfig_2.0.3     htmltools_0.5.9
```
