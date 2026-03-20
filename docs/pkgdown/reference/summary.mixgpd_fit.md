# Summarize posterior draws from a one-arm fitted model

`summary.mixgpd_fit()` computes posterior summaries for monitored model
parameters.

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

## Details

The returned table is a parameter-level summary of the posterior draws,
not a predictive summary. Use
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md)
for posterior predictive quantities such as densities, survival
probabilities, quantiles, and means.

The summary respects the stored truncation metadata and reports WAIC if
it was requested during MCMC.

## See also

[`print.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/print.mixgpd_fit.md),
[`params`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/params.md),
[`predict.mixgpd_fit`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/predict.mixgpd_fit.md),
[`ess_summary`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/ess_summary.md).

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
