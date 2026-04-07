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

## Details

This helper powers the one-arm summary methods. It extracts the retained
draw matrix, chooses a default set of non-redundant parameters when
`pars` is not supplied, and computes posterior means, standard
deviations, quantiles, effective sample sizes, and Gelman diagnostics
when available.

The resulting table is parameter oriented rather than prediction
oriented. It is the internal workhorse behind
[`summary.mixgpd_fit()`](https://arnabaich96.github.io/CausalMixGPD/pkgdown/reference/summary.mixgpd_fit.md).
