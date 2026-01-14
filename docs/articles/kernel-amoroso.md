# Kernel: Amoroso

``` r
library(DPmixGPD)
library(nimble)
use_cached_fit <- TRUE
.fit_path <- resolve_extdata_file
fit_small <- readRDS(.fit_path("fit_small.rds"))
w <- c(0.6, 0.4)
loc <- c(0.0, 0.5)
scale <- c(1.0, 1.4)
shape1 <- c(1.0, 1.0)
shape2 <- c(2.0, 3.0)

x <- 1.0
p <- 0.9

dAmorosoMix(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2, log = FALSE)
#> [1] 0.5459161
pAmorosoMix(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
            lower.tail = TRUE, log.p = FALSE)
#> [1] 0.3970851
qAmorosoMix(p, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2)
#> [1] 2.09016

threshold <- 1.2
tail_scale <- 0.9
tail_shape <- 0.2

dAmorosoMixGpd(x, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
               threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 0.5459161
qAmorosoMixGpd(0.99, w = w, loc = loc, scale = scale, shape1 = shape1, shape2 = shape2,
               threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 6.521116

set.seed(1)
y <- rgamma(30, shape = 2, scale = 1)

bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "amoroso",
  GPD = TRUE,
  components = 6
)

bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "amoroso"
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
#> [1] nimble_1.4.0   DPmixGPD_0.0.6
#> 
#> loaded via a namespace (and not attached):
#>  [1] cli_3.6.5           knitr_1.51          rlang_1.1.6        
#>  [4] xfun_0.55           otel_0.2.0          renv_1.1.5         
#>  [7] textshaping_1.0.4   jsonlite_2.0.0      htmltools_0.5.9    
#> [10] pracma_2.4.6        ragg_1.5.0          sass_0.4.10        
#> [13] rmarkdown_2.30      grid_4.5.2          evaluate_1.0.5     
#> [16] jquerylib_0.1.4     fastmap_1.2.0       numDeriv_2016.8-1.1
#> [19] yaml_2.3.12         lifecycle_1.0.4     compiler_4.5.2     
#> [22] codetools_0.2-20    igraph_2.2.1        coda_0.19-4.1      
#> [25] fs_1.6.6            pkgconfig_2.0.3     htmlwidgets_1.6.4  
#> [28] rstudioapi_0.17.1   lattice_0.22-7      systemfonts_1.3.1  
#> [31] digest_0.6.39       R6_2.6.1            parallel_4.5.2     
#> [34] magrittr_2.0.4      bslib_0.9.0         tools_4.5.2        
#> [37] pkgdown_2.2.0       cachem_1.1.0        desc_1.4.3
```
