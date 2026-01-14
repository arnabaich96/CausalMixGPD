# Kernel: Normal

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
w <- c(0.4, 0.6)
mean <- c(-1, 1)
sd <- c(0.7, 1.2)

x <- 0.5
p <- 0.9
```

``` r
dNormMix(x, w = w, mean = mean, sd = sd, log = FALSE)
#> [1] 0.2058354
pNormMix(x, w = w, mean = mean, sd = sd, lower.tail = TRUE, log.p = FALSE)
#> [1] 0.5966518
qNormMix(p, w = w, mean = mean, sd = sd)
#> [1] 2.160916
```

``` r
threshold <- 0.2
tail_scale <- 1.0
tail_shape <- 0.2
```

``` r
dNormMixGpd(x, w = w, mean = mean, sd = sd,
            threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 1e-300
qNormMixGpd(0.99, w = w, mean = mean, sd = sd,
            threshold = threshold, tail_scale = tail_scale, tail_shape = tail_shape)
#> [1] 5.979779
```

``` r
set.seed(1)
y <- abs(rnorm(30)) + 0.1
```

``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  J = 6
)
```

``` r
bundle$spec$meta
#> $backend
#> [1] "sb"
#> 
#> $kernel
#> [1] "normal"
#> 
#> $GPD
#> [1] TRUE
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
library(ggplot2)
data.frame(y = y) |>
  ggplot(aes(x = y)) +
  geom_histogram(boundary = 0, bins = 30, fill = "steelblue", alpha = 0.6) +
  labs(title = "Normal kernel example data", x = "y", y = "Count")
```

![](kernel-normal_files/figure-html/normal-viz-histogram-1.png)

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
#> [1] testthat_3.3.1 ggplot2_4.0.1  nimble_1.4.0   DPmixGPD_0.0.8
#> 
#> loaded via a namespace (and not attached):
#>  [1] sass_0.4.10         future_1.68.0       generics_0.1.4     
#>  [4] renv_1.1.5          lattice_0.22-7      listenv_0.10.0     
#>  [7] pracma_2.4.6        digest_0.6.39       magrittr_2.0.4     
#> [10] evaluate_1.0.5      grid_4.5.2          RColorBrewer_1.1-3 
#> [13] fastmap_1.2.0       jsonlite_2.0.0      brio_1.1.5         
#> [16] scales_1.4.0        codetools_0.2-20    numDeriv_2016.8-1.1
#> [19] textshaping_1.0.4   jquerylib_0.1.4     cli_3.6.5          
#> [22] rlang_1.1.6         parallelly_1.46.0   future.apply_1.20.1
#> [25] withr_3.0.2         cachem_1.1.0        yaml_2.3.12        
#> [28] otel_0.2.0          tools_4.5.2         parallel_4.5.2     
#> [31] coda_0.19-4.1       dplyr_1.1.4         globals_0.18.0     
#> [34] vctrs_0.6.5         R6_2.6.1            lifecycle_1.0.4    
#> [37] fs_1.6.6            htmlwidgets_1.6.4   ragg_1.5.0         
#> [40] pkgconfig_2.0.3     desc_1.4.3          pkgdown_2.2.0      
#> [43] bslib_0.9.0         pillar_1.11.1       gtable_0.3.6       
#> [46] glue_1.8.0          systemfonts_1.3.1   tidyselect_1.2.1   
#> [49] tibble_3.3.0        xfun_0.55           knitr_1.51         
#> [52] farver_2.1.2        htmltools_0.5.9     igraph_2.2.1       
#> [55] labeling_0.4.3      rmarkdown_2.30      compiler_4.5.2     
#> [58] S7_0.2.1
```
