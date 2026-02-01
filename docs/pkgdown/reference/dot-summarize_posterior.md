# Summarize posterior draws for selected parameters

Summarize posterior draws for selected parameters

## Usage

``` r
.summarize_posterior(object, pars = NULL, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- object:

  mixgpd_fit

- pars:

  character vector; if NULL uses all non-v parameters

- probs:

  quantiles to report

## Value

data.frame with mean/sd/quantiles + ess/rhat where available
