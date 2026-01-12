---
title: "Prediction & exports"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Prediction & exports}
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
library(ggplot2)
```


``` r
set.seed(3)
n <- 90
y <- sim_bulk_tail(n, tail_prob = 0.15)
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "normal",
  GPD = TRUE,
  J = 6,
  mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 2, seed = c(1, 2))
)
if (use_cached_fit) {
  fit <- fit_small
} else {
  fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
}
draws <- DPmixGPD:::.extract_draws_matrix(fit, drop_v = TRUE)
keep <- intersect(c("alpha", "threshold[1]", "tail_shape"), colnames(draws))
head(draws[, keep, drop = FALSE])
#>         alpha
#> [1,] 3.340427
#> [2,] 3.340427
#> [3,] 2.526394
#> [4,] 1.904067
#> [5,] 1.904067
#> [6,] 1.834272
```


``` r
pp <- predict(fit, type = "sample", nsim = 200)
draws_df <- data.frame(value = pp$fit[, 1])
ggplot(draws_df, aes(x = value)) +
  geom_histogram(bins = 30, fill = "#1f77b4", alpha = 0.6) +
  geom_density(data = data.frame(y = y), aes(x = y, y = ..density..), color = "#d62728") +
  labs(title = "Posterior predictive draws vs observed histogram", x = "y")
```

![Posterior predictive draws overlayed on data histogram.](D:\OneDrive - Florida State University\MyFSU_OneDrive\R-Codes\DPMGPD_package\DPmixGPD\vignettes\v08-prediction-and-exports_files/figure-html/predictive-draws-1.png)


``` r
dir.create("reports", showWarnings = FALSE, recursive = TRUE)
quant_df <- predict(fit, type = "quantile", p = c(.5, .9, .99), y = NULL)
export_table <- data.frame(
  tau = c(.5, .9, .99),
  quantile = c(quant_df$fit)
)
knitr::kable(export_table, caption = "Quantile export for reporting")
```



Table: Quantile export for reporting

|  tau|  quantile|
|----:|---------:|
| 0.50|  2.444721|
| 0.90|  9.102755|
| 0.99| 10.154704|



``` r
write.csv(export_table, file = "reports/quantiles.csv", row.names = FALSE)
```


``` r
recipes <- list(
  "Quantile curve" = function(newdata) predict(fit, newdata = newdata, type = "quantile", p = c(.5, .9, .99)),
  "Tail index surface" = function(newdata) predict(fit, newdata = newdata, type = "quantile", p = .995),
  "Tail exceedance rate" = function(newdata, ygrid) predict(fit, newdata = newdata, type = "survival", y = ygrid)
)
recipes
#> $`Quantile curve`
#> function (newdata) 
#> predict(fit, newdata = newdata, type = "quantile", p = c(0.5, 
#>     0.9, 0.99))
#> <environment: 0x0000018713940158>
#> 
#> $`Tail index surface`
#> function (newdata) 
#> predict(fit, newdata = newdata, type = "quantile", p = 0.995)
#> <environment: 0x0000018713940158>
#> 
#> $`Tail exceedance rate`
#> function (newdata, ygrid) 
#> predict(fit, newdata = newdata, type = "survival", y = ygrid)
#> <environment: 0x0000018713940158>
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
