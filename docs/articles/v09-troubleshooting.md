# Troubleshooting and Common Errors

What you’ll learn: how to interpret common warnings/errors, when to
check dimensions, and how to keep posterior tails stable.

## Not fully initialized

This note from NIMBLE is normal on complex models; it flags variables
not touched yet. Always run `fit$model$initializeInfo()` to confirm
there are no truly missing nodes. The short example below logs the
message and shows how to inspect:

``` r
bundle <- build_nimble_bundle(y = abs(rnorm(40)) + 0.1, backend = "sb", kernel = "normal", J = 5)
if (use_cached_fit) {
  fit <- fit_small
} else {
  fit <- run_mcmc_bundle_manual(bundle, show_progress = FALSE)
}
fit$bundle$code  # ensures model objects exist
#> NULL
```

When the warning persists, check `bundle$dimensions` and explicit
initial values in `bundle$inits` to ensure every latent vector has
compatible length.

## Dimension mismatch

Mixing data with different numbers of covariates quickly raises
dimension errors. Always confirm
`ncol(newdata) == ncol(bundle$design$data$X)` before calling
[`predict()`](https://rdrr.io/r/stats/predict.html).

``` r
bad_X <- matrix(1, nrow = 5, ncol = 1)
stopifnot(ncol(bad_X) == ncol(bundle$dimensions$X %||% matrix()))
```

## `'object "v" not found'` and stick variables

This class of errors arises when the `components` truncation is too
small or `epsilon` drives many weight draws to zero. Remedy: increase
`components` (your $`J`$) and raise `epsilon` slightly. The following
histogram shows posterior weight spread:

``` r
ws <- draw_w <- DPmixGPD:::.extract_draws_matrix(fit, drop_v = TRUE)
weight_cols <- grep("^w\\[[0-9]+\\]$", colnames(draw_w), value = TRUE)
weight_df <- data.frame(weight = c(draw_w[, weight_cols]))
ggplot(weight_df, aes(x = weight)) +
  geom_histogram(bins = 25, fill = "#1f77b4", color = "white") +
  labs(title = "Posterior weight distribution (some components may collapse)")
```

![Posterior weights that survived
truncation.](v09-troubleshooting_files/figure-html/weight-hist-1.png)

Posterior weights that survived truncation.

## Slow mixing

If trace plots show drift, lengthen `nburnin`, increase `thin`, or
reduce `niter` under raw CPU pressure. Try the `plot(fit)` diagnostics
above for thresholds and tail shape, and use
`plot(fit, family = "traceplot", params = "alpha")` to monitor
concentration.

## Tail instability

When tail shape/scale oscillate, the usual remedies are:

- **Prior sensitivity:** tighten `tail_shape` priors (e.g.,
  `param_specs = list(tail_shape = list(mode = "fixed", value = 0.2))`).
- **More exceedances:** raise `threshold` by setting a higher initial
  guess or increasing `J`.
- **GPD flag:** temporarily set `GPD = FALSE` to ensure your bulk is
  stable before reintroducing the tail.

## Decision chart

``` r
symptoms <- data.frame(
  symptom = c("Dimension mismatch", "Not fully initialized", "Tail shape blows up", "Slow mixing"),
  cause = c("newdata matrix columns differ", "latent nodes not set before compile", "too few tail exceedances", "thin/nchains too small"),
  fix = c("match columns/newdata", "provide explicit inits", "increase components/J or seed", "increase nburnin/thin or reduce autocorr")
)
knitr::kable(symptoms)
```

| symptom | cause | fix |
|:---|:---|:---|
| Dimension mismatch | newdata matrix columns differ | match columns/newdata |
| Not fully initialized | latent nodes not set before compile | provide explicit inits |
| Tail shape blows up | too few tail exceedances | increase components/J or seed |
| Slow mixing | thin/nchains too small | increase nburnin/thin or reduce autocorr |

## Prediction example

``` r
quant <- predict(fit, type = "quantile", p = c(.5, .9, .99))
quant
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
#>  [1] sass_0.4.10         future_1.68.0       generics_0.1.4     
#>  [4] renv_1.1.5          lattice_0.22-7      listenv_0.10.0     
#>  [7] pracma_2.4.6        digest_0.6.39       magrittr_2.0.4     
#> [10] evaluate_1.0.5      grid_4.5.2          RColorBrewer_1.1-3 
#> [13] fastmap_1.2.0       jsonlite_2.0.0      scales_1.4.0       
#> [16] codetools_0.2-20    numDeriv_2016.8-1.1 textshaping_1.0.4  
#> [19] jquerylib_0.1.4     cli_3.6.5           rlang_1.1.6        
#> [22] parallelly_1.46.0   future.apply_1.20.1 withr_3.0.2        
#> [25] cachem_1.1.0        yaml_2.3.12         otel_0.2.0         
#> [28] tools_4.5.2         parallel_4.5.2      coda_0.19-4.1      
#> [31] globals_0.18.0      vctrs_0.6.5         R6_2.6.1           
#> [34] lifecycle_1.0.4     fs_1.6.6            htmlwidgets_1.6.4  
#> [37] ragg_1.5.0          pkgconfig_2.0.3     desc_1.4.3         
#> [40] pillar_1.11.1       pkgdown_2.2.0       bslib_0.9.0        
#> [43] gtable_0.3.6        glue_1.8.0          systemfonts_1.3.1  
#> [46] tidyselect_1.2.1    tibble_3.3.0        xfun_0.55          
#> [49] rstudioapi_0.17.1   knitr_1.51          farver_2.1.2       
#> [52] htmltools_0.5.9     igraph_2.2.1        labeling_0.4.3     
#> [55] rmarkdown_2.30      compiler_4.5.2      S7_0.2.1
```
