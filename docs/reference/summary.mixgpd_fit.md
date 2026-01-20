# Summarize a MixGPD fitted object

Summarize a MixGPD fitted object

## Usage

``` r
# S3 method for class 'mixgpd_fit'
summary(object, pars = NULL, probs = c(0.025, 0.5, 0.975), ...)
```

## Arguments

- object:

  A fitted object of class `"mixgpd_fit"`.

- pars:

  Optional character vector of parameters to summarize. If NULL,
  summarize all (excluding v's).

- probs:

  Numeric vector of quantiles to report.

- ...:

  Unused.

## Value

An object of class `"mixgpd_summary"`.

## Examples

``` r
if (FALSE) { # \dontrun{
y <- abs(stats::rnorm(50)) + 0.1
bundle <- build_nimble_bundle(y = y, backend = "sb", kernel = "normal",
                             GPD = TRUE, components = 6,
                             mcmc = list(niter = 200, nburnin = 50, thin = 1, nchains = 1))
fit <- run_mcmc_bundle_manual(bundle)
summary(fit, pars = c("alpha", "threshold"))
} # }
```
