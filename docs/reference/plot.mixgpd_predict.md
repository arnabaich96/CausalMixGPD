# Plot prediction results

Generates type-specific visualizations for prediction objects returned
by
[`predict.mixgpd_fit()`](https://arnabaich96.github.io/DPmixGPD/reference/predict.mixgpd_fit.md).
Each prediction type produces a tailored plot:

- `quantile`: Quantile indices vs estimates with credible intervals

- `sample`: Histogram of samples with density overlay

- `mean`: Histogram density with posterior mean vertical line and CI
  bounds

- `density`: Density values vs evaluation points

- `survival`: Survival function (decreasing y values)

## Usage

``` r
# S3 method for class 'mixgpd_predict'
plot(x, y = NULL, ...)
```

## Arguments

- x:

  A prediction object returned by
  [`predict.mixgpd_fit()`](https://arnabaich96.github.io/DPmixGPD/reference/predict.mixgpd_fit.md).

- y:

  Ignored; included for S3 compatibility.

- ...:

  Additional arguments passed to ggplot2 functions.

## Value

Invisibly returns the ggplot object.

## Examples

``` r
# \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
#> [MCMC] Creating NIMBLE model...
#> Defining model
#>   [Note] 'P' is provided in 'constants' but not used in the model code and is being ignored.
#> Building model
#> [ERROR] Failed to create NIMBLE model:
#> Error in getNimbleOption("determinePredictiveNodesInModel"): could not find function "getNimbleOption"

# Quantile prediction with plot
pred_q <- predict(fit, type = "quantile", index = c(0.25, 0.5, 0.75))
#> Error: object 'fit' not found
plot(pred_q)
#> Error: object 'pred_q' not found

# Sample prediction with plot
pred_s <- predict(fit, type = "sample", nsim = 500)
#> Error: object 'fit' not found
plot(pred_s)
#> Error: object 'pred_s' not found

# Mean prediction with plot
pred_m <- predict(fit, type = "mean", nsim_mean = 300)
#> Error: object 'fit' not found
plot(pred_m)
#> Error: object 'pred_m' not found
# }
```
