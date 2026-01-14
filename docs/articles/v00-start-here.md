# Start Here

``` r
bulk <- data.frame(x = seq(0, 10, length.out = 200))
bulk$y <- dlnorm(bulk$x, meanlog = 0.5, sdlog = 0.4) * 0.8
tail <- data.frame(x = seq(8, 20, length.out = 3))
tail$y <- vapply(
  tail$x,
  DPmixGPD::dGpd,
  numeric(1),
  threshold = 10,
  scale = 2,
  shape = 0.3
)
ggplot() +
  geom_area(data = bulk, aes(x = x, y = y), fill = "#66c2a5", alpha = 0.7) +
  geom_line(data = tail, aes(x = x, y = y), color = "#fc8d62", size = 1.2) +
  geom_vline(xintercept = 10, linetype = "dashed") +
  annotate("text", x = 12, y = 0.15, label = "GPD tail", color = "#fc8d62") +
  annotate("text", x = 4, y = 0.25, label = "DP mixture bulk", color = "#66c2a5") +
  labs(x = "Outcome", y = "Density")
```

![DPmixGPD concept: flexible bulk + GPD tail stitched at a
threshold.](v00-start-here_files/figure-html/start-viz-concept-1.png)

DPmixGPD concept: flexible bulk + GPD tail stitched at a threshold.

``` r
set.seed(7)
y <- sim_bulk_tail(90, tail_prob = 0.2)
J <- 6
```

``` r
bundle <- build_nimble_bundle(
  y = y,
  backend = "sb",
  kernel = "lognormal",
  GPD = TRUE,
  J = J,
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
summary(fit, component = "tail")
#> MixGPD summary | backend: Stick-Breaking Process | kernel: Normal Distribution | GPD tail: FALSE | epsilon: 0.025
#> n = 80 | components = 3
#> Summary
#> Initial components: 3 | Components after truncation: 3
#> 
#> Summary table
#>   parameter  mean    sd q0.025 q0.500 q0.975    ess
#>  weights[1] 0.638 0.081  0.524  0.625  0.775  3.535
#>  weights[2] 0.211 0.033  0.150  0.212  0.251  8.389
#>  weights[3] 0.154 0.055  0.049  0.162  0.225  3.421
#>       alpha 1.945 0.818  0.575  1.904  3.341 13.115
#>     mean[1] 1.150 0.154  0.876  1.144  1.404  9.376
#>     mean[2] 5.956 3.400  2.343  6.970 11.420 15.873
#>     mean[3] 6.067 3.332  2.530  3.815 11.671 13.235
#>       sd[1] 3.027 1.226  1.385  2.780  5.462  8.118
#>       sd[2] 1.110 1.316  0.023  0.059  3.947  3.470
#>       sd[3] 1.356 1.652  0.029  0.587  5.270 15.139
```

``` r
pred <- predict(fit, type = "survival", y = seq(0, max(y) + 5, length.out = 100))
surv_df <- data.frame(y = pred$grid, survival = as.numeric(pred$fit[1, ]))
ggplot(surv_df, aes(x = y, y = survival)) +
  geom_line(color = "#7b3294") +
  labs(title = "Posterior survival curve", y = "Survival", x = "y")
```

![Density prediction on held-out grid (posterior
mean).](v00-start-here_files/figure-html/start-s3-predict-1.png)

Density prediction on held-out grid (posterior mean).

``` r
predict(fit, type = "quantile", p = c(.5, .8, .95))
#> $fit
#>          [,1]     [,2]     [,3]
#> [1,] 2.444721 7.007227 9.279874
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
#> [1] 0.50 0.80 0.95
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
