# Residual-style diagnostics for a MixGPD fit

Currently supports PIT (probability integral transform) residuals only
when a working `predict(type="survival")` is available for the training
data.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
residuals(object, type = c("pit"), ...)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- type:

  Residual type. Only `"pit"` is implemented here.

- ...:

  Unused.

## Value

Numeric vector of residuals (length nobs).

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
pit <- residuals(fit, type = "pit")
} # }
```
