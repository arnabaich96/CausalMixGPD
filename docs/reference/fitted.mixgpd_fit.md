# Fitted values and residuals for a MixGPD fit

Computes fitted values and residuals on the original training data.
Returns a data frame with point estimates, credible intervals, and
residuals. For unconditional models (no covariates), returns the
population mean replicated for all observations. For conditional models,
returns individual predictions.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
fitted(
  object,
  type = c("location", "mean", "median", "quantile"),
  p = 0.5,
  level = 0.95,
  seed = 1,
  ...
)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- type:

  Which fitted location to return: mean, median, quantile, or both
  (`"location"`).

- p:

  Quantile level used when `type = "quantile"`.

- level:

  Credible level for confidence intervals (default 0.95 for 95% credible
  intervals).

- seed:

  Random seed used for deterministic fitted values.

- ...:

  Unused.

## Value

A data frame with columns: `fit` (point estimates), `lower` (lower
credible bound), `upper` (upper credible bound), and `residuals` (y -
fit).

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
#>   [Note] Registering 'dNormGpd' as a distribution based on its use in BUGS code. If you make changes to the nimbleFunctions for the distribution, you must call 'deregisterDistributions' before using the distribution in BUGS code for those changes to take effect.
#> Building model
#> [ERROR] Failed to create NIMBLE model:
#> Error in getNimbleOption("determinePredictiveNodesInModel"): could not find function "getNimbleOption"
fitted(fit)
#> Error: object 'fit' not found
fitted(fit, level = 0.90)
#> Error: object 'fit' not found
# }
```
