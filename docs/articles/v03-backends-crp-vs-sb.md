# Backends: CRP vs SB

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
fit_small_crp <- readRDS(.fit_path("fit_small_crp.rds"))
library(ggplot2)
```

``` r
y <- sim_bulk_tail(n = 150, tail_prob = 0.08, seed = 101)
```

``` r
common_args <- list(
  y = y,
  kernel = "lognormal",
  GPD = TRUE,
  J = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 2, seed = c(1, 2))
)
```

``` r
if (use_cached_fit) {
  fit_sb <- fit_small
  sb_time <- c(elapsed = NA_real_)
} else {
  sb_time <- system.time({
    bundle_sb <- do.call(build_nimble_bundle, c(list(backend = "sb"), common_args))
    fit_sb <- run_mcmc_bundle_manual(bundle_sb)
  })
}
```

``` r
if (use_cached_fit) {
  fit_crp <- fit_small_crp
  crp_time <- c(elapsed = NA_real_)
} else {
  crp_time <- system.time({
    bundle_crp <- do.call(build_nimble_bundle, c(list(backend = "crp"), common_args))
    fit_crp <- run_mcmc_bundle_manual(bundle_crp)
  })
}
```

``` r
system_time <- data.frame(
  backend = c("SB", "CRP"),
  elapsed = c(sb_time[["elapsed"]], crp_time[["elapsed"]])
)
system_time
#>   backend elapsed
#> 1      SB      NA
#> 2     CRP      NA
```

``` r
q_sb <- predict(fit_sb, type = "quantile", p = c(0.5, 0.9))
q_crp <- predict(fit_crp, type = "quantile", p = c(0.5, 0.9))
```

``` r
data.frame(
  backend = rep(c("SB", "CRP"), each = 2),
  prob = rep(c(0.5, 0.9), 2),
  quantile = c(as.numeric(q_sb$fit), as.numeric(q_crp$fit))
) |>
  ggplot(aes(x = prob, y = quantile, color = backend)) +
  geom_point() +
  geom_line() +
  labs(title = "Quantiles by backend", y = "Quantile", x = "Probability")
```

![Quantiles by
backend.](v03-backends-crp-vs-sb_files/figure-html/backend-viz-quantiles-1.png)

Quantiles by backend.

``` r
quantiles_sb <- predict(fit_sb, type = "quantile", p = c(.95, .99))
quantiles_crp <- predict(fit_crp, type = "quantile", p = c(.95, .99))
list(sb = quantiles_sb, crp = quantiles_crp)
#> $sb
#> $sb$fit
#>          [,1]    [,2]
#> [1,] 9.279874 10.1547
#> 
#> $sb$lower
#> NULL
#> 
#> $sb$upper
#> NULL
#> 
#> $sb$type
#> [1] "quantile"
#> 
#> $sb$grid
#> [1] 0.95 0.99
#> 
#> 
#> $crp
#> $crp$fit
#>          [,1]     [,2]
#> [1,] 6.813156 7.436865
#> 
#> $crp$lower
#> NULL
#> 
#> $crp$upper
#> NULL
#> 
#> $crp$type
#> [1] "quantile"
#> 
#> $crp$grid
#> [1] 0.95 0.99
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
#> [1] ggplot2_4.0.1  nimble_1.4.0   DPmixGPD_0.0.8
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
#> [28] parallel_4.5.2      coda_0.19-4.1       dplyr_1.1.4        
#> [31] globals_0.18.0      vctrs_0.6.5         R6_2.6.1           
#> [34] lifecycle_1.0.5     fs_1.6.6            htmlwidgets_1.6.4  
#> [37] ragg_1.5.0          pkgconfig_2.0.3     desc_1.4.3         
#> [40] pkgdown_2.1.3       bslib_0.9.0         pillar_1.11.1      
#> [43] gtable_0.3.6        glue_1.8.0          systemfonts_1.2.3  
#> [46] xfun_0.52           tibble_3.3.0        tidyselect_1.2.1   
#> [49] knitr_1.50          farver_2.1.2        htmltools_0.5.8.1  
#> [52] igraph_2.2.1        labeling_0.4.3      rmarkdown_2.30     
#> [55] compiler_4.5.2      S7_0.2.1
```
