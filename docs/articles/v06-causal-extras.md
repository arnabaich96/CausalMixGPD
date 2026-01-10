# Causal Extras: ATE, CATE, and Comparators

What you’ll learn: how CQTE fits into the broader causal toolkit, how to
compare it with ATE/CATE estimands, and how to add simple baselines such
as IPW quantiles.

## Estimand relationships

- CQTE subtracts conditional quantiles at levels `tau` for treated
  (`trt`) vs control (`con`) arms at the same covariate point.
- ATE averages the mean outcomes; CATE averages conditional means. CQTE
  instead tracks tail behavior that matter for extremes.
- If CQTE is flat across `tau`, the treatment effect does not depend on
  extremeness; rising curves point to tail dominance.

## Minimal causal-comparison example

``` r
set.seed(10)
data <- sim_causal_cqte(n = 200)
X <- data$X
bundle <- build_causal_bundle(
  y = data$y,
  X = X,
  T = data$t,
  backend = "sb",
  kernel = "normal",
  GPD = FALSE,
  J = 5,
  mcmc_outcome = list(niter = 200, nburnin = 50, thin = 3, nchains = 2, seed = c(1, 2)),
  mcmc_ps = list(niter = 200, nburnin = 50, thin = 3, nchains = 2, seed = c(3, 4))
)
if (use_cached_fit) {
  fit <- fit_causal_small
} else {
  fit <- run_mcmc_causal(bundle)
}
cq <- cqte(fit, probs = c(0.5, 0.9, 0.99), newdata = head(X, 3))
cq
#> $fit
#>             [,1]       [,2]      [,3]
#> [1,] -0.07887929 -0.1838758 -0.772090
#> [2,] -0.56656238 -0.6588275 -2.010648
#> [3,] -4.11858969 -5.4210058 -5.891019
#> 
#> $lower
#> NULL
#> 
#> $upper
#> NULL
#> 
#> $grid
#> [1] 0.50 0.90 0.99
#> 
#> $trt
#> $trt$fit
#>           [,1]      [,2]      [,3]
#> [1,]  3.797493  3.999549 4.0818921
#> [2,]  1.177050  1.261966 1.5440782
#> [3,] -3.677590 -2.197071 0.2013969
#> 
#> $trt$lower
#> NULL
#> 
#> $trt$upper
#> NULL
#> 
#> $trt$type
#> [1] "quantile"
#> 
#> $trt$grid
#> [1] 0.50 0.90 0.99
#> 
#> 
#> $con
#> $con$fit
#>           [,1]     [,2]     [,3]
#> [1,] 3.8763726 4.183425 4.853982
#> [2,] 1.7436125 1.920793 3.554727
#> [3,] 0.4409999 3.223935 6.092416
#> 
#> $con$lower
#> NULL
#> 
#> $con$upper
#> NULL
#> 
#> $con$type
#> [1] "quantile"
#> 
#> $con$grid
#> [1] 0.50 0.90 0.99
#> 
#> 
#> $type
#> [1] "cqte"
#> 
#> attr(,"class")
#> [1] "dpmixgpd_cqte"
```

The cached fit keeps `GPD = FALSE` for speed. Turn it on and adjust the
kernel when you need tail-focused causal contrasts.

## Diagnostic plot: treatment densities

``` r
df_plot <- data.frame(y = data$y, arm = ifelse(data$t == 1, "trt", "con"))
ggplot(df_plot, aes(x = y, fill = arm)) +
  geom_density(alpha = 0.4) +
  labs(title = "Outcome densities by arm", x = "Outcome y", fill = "Arm")
```

![](v06-causal-extras_files/figure-html/causal-plot-1.png)

## Comparison table and alternative baseline

| Estimand | Description |
|----|----|
| CQTE | Difference between treatment and control quantiles (`trt` minus `con`) at tail probability `tau`. |
| CATE | Mean difference conditional on covariates (special case of CQTE at `tau=0.5` when tails align). |
| ATE | Average of CATE across the empirical covariate distribution. |

``` r
arm_tbl <- data.frame(y = data$y, treatment = ifelse(data$t == 1, "trt", "con"))
ipw_q <- arm_tbl |>
  group_by(treatment) |>
  summarize(median = quantile(y, 0.5), `90%` = quantile(y, 0.9))
ipw_q
#> # A tibble: 2 × 3
#>   treatment median `90%`
#>   <chr>      <dbl> <dbl>
#> 1 con         3.35  7.04
#> 2 trt         5.44  9.02
```

## Prediction handoff

``` r
pred_grid <- data.frame(x1 = seq(-1, 1, length.out = 5), x2 = rep(0, 5), x3 = 0)
cq_pred <- cqte(fit, probs = c(0.9, 0.99), newdata = pred_grid)
cq_pred
#> $fit
#>            [,1]      [,2]
#> [1,] -3.3104495 -4.672876
#> [2,] -1.7213333 -3.521082
#> [3,] -0.2961406 -2.533016
#> [4,]  1.0014507 -1.501489
#> [5,]  2.0640500 -0.321192
#> 
#> $lower
#> NULL
#> 
#> $upper
#> NULL
#> 
#> $grid
#> [1] 0.90 0.99
#> 
#> $trt
#> $trt$fit
#>            [,1]      [,2]
#> [1,] -1.3811586 0.2257941
#> [2,] -0.6870986 0.4167933
#> [3,]  0.0442925 0.6607280
#> [4,]  1.1661257 1.3300628
#> [5,]  2.2951368 2.4149671
#> 
#> $trt$lower
#> NULL
#> 
#> $trt$upper
#> NULL
#> 
#> $trt$type
#> [1] "quantile"
#> 
#> $trt$grid
#> [1] 0.90 0.99
#> 
#> 
#> $con
#> $con$fit
#>           [,1]     [,2]
#> [1,] 1.9292909 4.898670
#> [2,] 1.0342348 3.937876
#> [3,] 0.3404331 3.193744
#> [4,] 0.1646750 2.831552
#> [5,] 0.2310868 2.736159
#> 
#> $con$lower
#> NULL
#> 
#> $con$upper
#> NULL
#> 
#> $con$type
#> [1] "quantile"
#> 
#> $con$grid
#> [1] 0.90 0.99
#> 
#> 
#> $type
#> [1] "cqte"
#> 
#> attr(,"class")
#> [1] "dpmixgpd_cqte"
```

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
#> [1] dplyr_1.1.4    ggplot2_4.0.1  nimble_1.4.0   DPmixGPD_0.0.8
#> 
#> loaded via a namespace (and not attached):
#>  [1] utf8_1.2.6          sass_0.4.10         future_1.68.0      
#>  [4] generics_0.1.4      renv_1.1.5          lattice_0.22-7     
#>  [7] listenv_0.10.0      pracma_2.4.6        digest_0.6.39      
#> [10] magrittr_2.0.4      evaluate_1.0.5      grid_4.5.2         
#> [13] RColorBrewer_1.1-3  fastmap_1.2.0       jsonlite_2.0.0     
#> [16] scales_1.4.0        codetools_0.2-20    numDeriv_2016.8-1.1
#> [19] textshaping_1.0.4   jquerylib_0.1.4     cli_3.6.5          
#> [22] rlang_1.1.6         parallelly_1.46.0   future.apply_1.20.1
#> [25] withr_3.0.2         cachem_1.1.0        yaml_2.3.12        
#> [28] otel_0.2.0          tools_4.5.2         parallel_4.5.2     
#> [31] coda_0.19-4.1       globals_0.18.0      vctrs_0.6.5        
#> [34] R6_2.6.1            lifecycle_1.0.4     fs_1.6.6           
#> [37] htmlwidgets_1.6.4   ragg_1.5.0          pkgconfig_2.0.3    
#> [40] desc_1.4.3          pillar_1.11.1       pkgdown_2.2.0      
#> [43] bslib_0.9.0         gtable_0.3.6        glue_1.8.0         
#> [46] systemfonts_1.3.1   tidyselect_1.2.1    tibble_3.3.0       
#> [49] xfun_0.55           rstudioapi_0.17.1   knitr_1.51         
#> [52] farver_2.1.2        htmltools_0.5.9     igraph_2.2.1       
#> [55] labeling_0.4.3      rmarkdown_2.30      compiler_4.5.2     
#> [58] S7_0.2.1
```
