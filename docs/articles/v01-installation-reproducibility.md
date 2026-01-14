# Installation & Reproducibility

``` r
y <- sim_bulk_tail(n = 120, seed = 42)
```

``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "gamma",
  GPD = TRUE,
  J = 5,
  mcmc = list(niter = 200, nburnin = 50, thin = 2, nchains = 2, seed = c(1, 2))
)
```

``` r
if (use_cached_fit) {
  fit <- fit_small
} else {
  fit <- run_mcmc_bundle_manual(bundle)
}
print(fit)
#> MixGPD fit | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: FALSE
#> n = 80 | components = 3 | epsilon = 0.025
#> MCMC: niter=60, nburnin=20, thin=1, nchains=1 
#> Fit
#> Use summary() for posterior summaries; plot() for diagnostics; predict() for predictions.
```

``` r
data.frame(y = y) |>
  ggplot(aes(x = y)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Input data density", x = "y", y = "Density")
```

![](v01-installation-reproducibility_files/figure-html/install-viz-data-1.png)

``` r
preds <- predict(fit, type = "quantile", p = c(0.5, 0.9, 0.95))
print(preds)
#> $fit
#>          [,1]     [,2]     [,3]
#> [1,] 2.444721 9.102755 9.279874
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
#> [1] 0.50 0.90 0.95
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
