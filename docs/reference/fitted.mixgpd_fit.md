# Fitted values for a MixGPD fit

Returns a simple fitted value summary on the training data. If an engine
provides a fitted function at `object$engine$fitted`, it will be used.
Otherwise, returns the empirical median of `y` replicated.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
fitted(object, ...)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- ...:

  Unused.

## Value

Numeric vector of length `nobs(object)`.

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
fitted(fit)
} # }
```
