# Print a MixGPD fitted object

Print a MixGPD fitted object

## Usage

``` r
# S3 method for class 'mixgpd_fit'
print(x, ...)
```

## Arguments

- x:

  A fitted object of class `"mixgpd_fit"`.

- ...:

  Unused.

## Value

`x` invisibly.

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
print(fit)
#> Error: object 'fit' not found
# }
```
