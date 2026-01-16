# Fitted values and residuals for a MixGPD fit

Computes fitted values and residuals on the original training data.
Returns a data frame with point estimates, credible intervals, and
residuals. For unconditional models (no covariates), returns the
population mean replicated for all observations. For conditional models,
returns individual predictions.

## Usage

``` r
# S3 method for class 'mixgpd_fit'
fitted(object, level = 0.95, ...)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- level:

  Credible level for confidence intervals (default 0.95 for 95% credible
  intervals).

- ...:

  Unused.

## Value

A data frame with columns:

- fit:

  Point estimates (posterior means).

- lower:

  Lower credible bound.

- upper:

  Upper credible bound.

- residuals:

  Residuals (y - fit).

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
fitted(fit)
fitted(fit, level = 0.90)
} # }
```
