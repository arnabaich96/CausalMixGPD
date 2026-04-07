# Summarize a propensity score fit

`summary.causalmixgpd_ps_fit()` returns posterior summaries for the
monitored PS-model parameters.

## Usage

``` r
# S3 method for class 'causalmixgpd_ps_fit'
summary(object, pars = NULL, probs = c(0.025, 0.5, 0.975), ...)
```

## Arguments

- object:

  A `"causalmixgpd_ps_fit"` object.

- pars:

  Optional character vector of PS parameters to summarize. If `NULL`,
  summarize all monitored parameters.

- probs:

  Numeric vector of posterior quantiles to report.

- ...:

  Unused.

## Value

An object of class `"summary.causalmixgpd_ps_fit"` with elements `model`
and `table`.

## Details

The summary is parameter based. For logit and probit models, it
summarizes the posterior draws of the coefficients that determine the
latent linear predictor, which is then mapped to \\e(x)\\ by the chosen
link function. For the naive Bayes option, it summarizes the
class-conditional parameters used to factorize the treatment-assignment
model.

This function does not compute fitted propensity scores for specific
covariate rows. It summarizes the posterior distribution of the PS model
itself, which is the nuisance model later used by causal prediction and
treatment-effect standardization.
