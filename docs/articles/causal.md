# Causal modeling quickstart

``` r
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else {
  library(DPmixGPD)
}
library(nimble)
use_cached_fit <- FALSE
.fit_path <- function(name) {
  path <- system.file("extdata", name, package = "DPmixGPD")
  if (path == "") path <- file.path("inst", "extdata", name)
  path
}
fit_causal_small <- NULL
if (use_cached_fit) {
  fit_causal_con <- readRDS(.fit_path("fit_causal_con.rds"))
  fit_causal_trt <- readRDS(.fit_path("fit_causal_trt.rds"))
  fit_causal_meta <- readRDS(.fit_path("fit_causal_meta.rds"))
  fit_causal_small <- list(
    ps_fit = NULL,
    outcome_fit = list(con = fit_causal_con, trt = fit_causal_trt),
    bundle = fit_causal_meta,
    call = NULL
  )
  class(fit_causal_small) <- "dpmixgpd_causal_fit"
}
```

``` r
set.seed(1)
dat <- sim_causal_qte(n = 120)
X <- dat$X
T <- dat$t
y <- dat$y

cb <- build_causal_bundle(
  y = y,
  X = X,
  T = T,
  backend = c("sb", "sb"),
  kernel = c("normal", "normal"),
  GPD = c(FALSE, FALSE),
  J = c(8, 8),
  mcmc_outcome = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1),
  mcmc_ps = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
)
```

``` r
if (use_cached_fit) {
  cf <- fit_causal_small
} else {
  cf <- run_mcmc_causal(cb, show_progress = FALSE)
}
summary(cf$outcome_fit$trt)
newdata <- X
```

``` r
qt <- qte(cf, probs = c(0.5, 0.9), newdata = newdata)
qt$fit

qs <- c(0.1, 0.5, 0.9)
qt2 <- qte(cf, probs = qs, newdata = newdata)
plot(qs, as.numeric(qt2$fit[1, ]), type = "b",
     xlab = "quantile", ylab = "QTE",
     main = "Quantile treatment effect")

# Posterior mean (ATE)
ate_res <- ate(cf, newdata = newdata)
head(ate_res$fit)
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
#> [1] DPmixGPD_0.0.6 testthat_3.3.1 nimble_1.4.0  
#> 
#> loaded via a namespace (and not attached):
#>  [1] cli_3.6.5           knitr_1.51          rlang_1.1.6        
#>  [4] xfun_0.55           otel_0.2.0          renv_1.1.5         
#>  [7] textshaping_1.0.4   jsonlite_2.0.0      htmltools_0.5.9    
#> [10] pracma_2.4.6        ragg_1.5.0          sass_0.4.10        
#> [13] brio_1.1.5          rmarkdown_2.30      grid_4.5.2         
#> [16] evaluate_1.0.5      jquerylib_0.1.4     fastmap_1.2.0      
#> [19] numDeriv_2016.8-1.1 yaml_2.3.12         lifecycle_1.0.4    
#> [22] compiler_4.5.2      igraph_2.2.1        fs_1.6.6           
#> [25] coda_0.19-4.1       pkgconfig_2.0.3     htmlwidgets_1.6.4  
#> [28] rstudioapi_0.17.1   lattice_0.22-7      systemfonts_1.3.1  
#> [31] digest_0.6.39       R6_2.6.1            parallel_4.5.2     
#> [34] magrittr_2.0.4      bslib_0.9.0         tools_4.5.2        
#> [37] pkgdown_2.2.0       cachem_1.1.0        desc_1.4.3
```
