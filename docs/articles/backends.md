# Backends: SB vs CRP

## Summary

DPmixGPD supports two Bayesian nonparametric backends:

- SB (stick-breaking): explicit mixture weights
- CRP (Chinese Restaurant Process): allocation-based representation

Both share the same kernel registry and prediction interface.

## Example data

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
set.seed(2)
N <- 60
y <- abs(rnorm(N)) + 0.1
```

## SB fit

``` r
if (use_cached_fit) {
  fit_sb <- fit_small
} else {
  fit_sb <- run_mcmc_bundle_manual(build_nimble_bundle(
    y = y,
    backend = "sb",
    kernel = "normal",
    J = 8,
    mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
  ), show_progress = FALSE)
}

summary(fit_sb)
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

## CRP fit

``` r
if (use_cached_fit) {
  fit_crp <- fit_small_crp
} else {
  fit_crp <- run_mcmc_bundle_manual(build_nimble_bundle(
    y = y,
    backend = "crp",
    kernel = "normal",
    J = 8,
    mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1, seed = 1)
  ), show_progress = FALSE)
}

summary(fit_crp)
#> MixGPD summary | backend: Chinese Restaurant Process | kernel: Normal Distribution | GPD tail: FALSE | epsilon: 0.025
#> n = 80 | components = 3
#> Summary
#> Initial components: 3 | Components after truncation: 3
#> 
#> Summary table
#>   parameter  mean    sd q0.025 q0.500 q0.975    ess
#>  weights[1] 0.542 0.055  0.462  0.537  0.663  7.671
#>  weights[2] 0.344 0.068  0.262  0.325  0.475  3.588
#>  weights[3] 0.175 0.033  0.102  0.175  0.217 13.487
#>       alpha 0.540 0.285  0.138  0.491  1.167 40.000
#>     mean[1] 1.092 0.153  0.852  1.094  1.325  5.262
#>     mean[2] 6.729 1.203  4.726  6.919  8.907 18.225
#>     mean[3] 2.983 0.288  2.596  2.953  3.557 10.855
#>       sd[1] 2.585 0.810  1.137  2.579  4.132 22.478
#>       sd[2] 0.053 0.015  0.028  0.049  0.080 40.000
#>       sd[3] 1.692 0.441  0.998  1.611  2.517 12.295
```

## Prediction comparison

``` r
q_sb <- predict(fit_sb, type = "quantile", p = c(0.5, 0.9))$fit
q_crp <- predict(fit_crp, type = "quantile", p = c(0.5, 0.9))$fit

rbind(sb = q_sb, crp = q_crp)
#>          [,1]     [,2]
#> [1,] 2.444721 9.102755
#> [2,] 3.316207 6.765119
```

## When to use each

- SB: you want explicit mixture weights and direct stick-breaking
  interpretation.
- CRP: you want allocation-based modeling with implicit weights.

## Computational tradeoffs

- SB uses explicit weights; CRP reconstructs weights from allocations.
- Both require a truncation level for NIMBLE code generation.
- Mixing behavior can differ by kernel and data.
