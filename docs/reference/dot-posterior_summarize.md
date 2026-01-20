# Summarize posterior draws (mean + quantiles)

Summarize posterior draws (mean + quantiles)

## Usage

``` r
.posterior_summarize(
  draws,
  probs = c(0.025, 0.5, 0.975),
  interval = "credible"
)
```

## Arguments

- draws:

  Numeric vector, matrix, or array with draws in last dimension.

- probs:

  Numeric quantile probs.

- interval:

  Character or NULL; `NULL` for no interval, `"credible"` for
  equal-tailed quantile intervals (default), `"hpd"` for highest
  posterior density intervals.

## Value

List with estimate, lower, upper, and q.
